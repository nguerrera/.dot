---
name: force-cleanup
description: Delete every agent/* branch in this repository, on the remote and locally, once GitHub reports nothing open here. Start it only when the user asks for it by name. The force is the user's assertion that nothing on those branches is still wanted, so the one thing checked against it is that no pull request is open; an open one stops the run whole, and the retry after the user has dealt with it is another invocation.
---

# Delete every leftover branch here

`cleanup` deletes a branch once its pull request finished. What it leaves is
every branch that never had one: pushed and never opened, or never pushed at
all. This deletes all of them, on the user's word that none of it is wanted.

**Start this only when the user asks for it by name.** Not a merge, not an
untidy branch listing, not the user saying they are finished with a branch. What
it deletes is work that never landed.

## The one check

The force claims that nothing here is outstanding. An open pull request is where
that claim is false, so check it.

```sh
gh pr list --state open --json url,isDraft,headRefName
```

An empty list is the go-ahead. Anything at all stops the run before a single
deletion.

- **A draft counts as open**, and `--state open` returns it. A draft is a pull
  request whose author has not said the work is finished.
- **The stop is the whole repository, not the one branch.** One open pull
  request says the user's account of what is going on is wrong.
- **`url` is what the report prints**, so ask for it here rather than building
  one from a number.

## The sweep

Four steps: every `agent/*` branch the remote carries, every one held locally
with its commit, then the remote set deleted and the local set deleted.

```sh
git ls-remote --heads origin 'refs/heads/agent/*'
git branch --list 'agent/*' --format='%(refname:short) %(objectname:short)'
git push origin --delete <branch> <branch> ...
git branch -D <branch> <branch> ...
```

Reading the remote's refs directly makes a fetch unnecessary and a stale
checkout harmless. Record the hashes both listings print before deleting
anything.

- **The local delete overrides the safety check every time.** A squash merge
  broke ancestry, so the guarded delete refuses every branch this workflow
  produces. The sides are independent: each is deleted where it is found.

- **A branch that is checked out cannot be deleted.** Get to `main` before the
  local pass, and where a worktree holds one:

  ```sh
  git worktree remove <path>
  git worktree prune
  ```

  A worktree carrying uncommitted work is a stop for that branch rather than a
  `--force`. Report it, leave both, carry on with the rest.

- **What each branch's pull request was is one more call, not one per branch:**

  ```sh
  gh pr list --state all --limit 200 --json url,state,headRefName
  ```

  Match on `headRefName`. A branch matching nothing had no pull request at all.

- **The pattern is the guard.** Nothing outside `agent/` is deleted on either
  side. A branch that is neither `main` nor under `agent/` is reported and left.

Each side is one deletion call however many branches it carries.

## What a deletion costs

Both sides go together, so neither is a backup for the other.

- **A branch that had a pull request stays reachable through it.** GitHub keeps
  `refs/pull/<n>/head` after the branch is gone, so
  `git fetch origin refs/pull/<n>/head` brings the commits back.
- **A branch that never had one leaves the hash, and sometimes the objects.**
  Where the local side existed, git holds the unreachable commit for
  `gc.pruneExpire`, two weeks by default, and `git fsck --unreachable` finds it.
  A branch only ever on the remote leaves the hash and nothing to resolve it
  against, which is why the report prints it.

## Report

One line per branch. No narration.

```
deleted  agent/4b71a2/spike  a01f3c9  local   never pushed
deleted  agent/9c03fe/nas    8f21a04  remote  no pull request
deleted  agent/d15e07/wip    b31f0c4  both    closed unmerged  <pull request URL>
```

**Which sides existed is a column.** A local branch was never pushed; a remote
one outlived a checkout somebody deleted.

**Name a pull request by URL rather than by number**, last on the line where its
length costs the columns nothing.

A run that stopped says so in place of the list and names every open pull
request by URL, with whether it is a draft.

## The retry is another invocation

A stopped run comes back only when the user asks again. Do not wait for the pull
request, do not poll, and do not offer to carry on afterwards. Merging it,
closing it, or going on working is the decision the stop hands back.
