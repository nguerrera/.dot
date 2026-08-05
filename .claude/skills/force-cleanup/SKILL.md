---
name: force-cleanup
description: Delete every agent/* branch in this repository, on the remote and locally, once GitHub reports nothing open here. Start it only when the user asks for it by name. The force is the user's assertion that nothing on those branches is still wanted, so the one thing checked against it is that no pull request is open; an open one stops the run whole, and the retry after the user has dealt with it is another invocation.
---

# Delete every leftover branch here

`cleanup` deletes a local branch once its pull request merged, and
`delete_branch_on_merge` had already taken the remote one. What it leaves is
every branch whose pull request was closed unmerged or never opened, on both
sides. Those accumulate across a stretch of not sweeping, and nothing else will
ever go looking for them.

This deletes all of them, in this repository, on the user's word that none of it
is wanted.

**Start this only when the user asks for it by name.** Nothing else is a
trigger: not a merge, not a branch listing that looks untidy, not the user
saying they are finished with a branch. What it deletes is work that never
landed, so an unprompted run can destroy the only copy of something the user was
never asked about.

## What the force is claiming

The claim is that nothing here is outstanding -- every change the user started
has landed or been given up on -- which makes every `agent/*` branch still
sitting on either side dead weight. Under that premise the deletions decide
nothing, so no branch is asked about one at a time and the name is where the
scope was acknowledged.

**The premise is checkable, so check it.** An open pull request is one where the
claim is false, and what is in flight is something the user forgot rather than
something they decided about.

## The one check

```sh
gh pr list --state open --json url,isDraft,headRefName
```

An empty list is the go-ahead. Anything at all stops the run before a single
deletion.

- **A draft counts as open**, and `--state open` returns it. A draft is a pull
  request whose author has not said the work is finished, which is what
  outstanding means.
- **The stop is the whole repository, not the one branch the pull request is
  on.** The premise being checked is that nothing here is going on, and one open
  pull request says the user's account of it is wrong. Nothing else is dead
  weight on evidence any more.
- **`url` is what the report prints**, so ask for it here rather than building
  one from a number.

## The sweep

```sh
git ls-remote --heads origin 'refs/heads/agent/*'
git branch --list 'agent/*' --format='%(refname:short) %(objectname:short)'
git push origin --delete <branch> <branch> ...
git branch -D <branch> <branch> ...
```

`ls-remote` reads the remote itself, so nothing has to be fetched first and a
stale checkout does not matter. Record the hashes both listings print before
deleting anything.

**`-D` takes a capital because a squash merge broke ancestry**, which is the
normal case for every branch this workflow produces rather than a hazard being
overridden here. The sides are independent: a branch can exist on one and not
the other, and each is deleted where it is found.

**A branch that is checked out cannot be deleted.** Get to `main` before the
local pass, and where a worktree holds one:

```sh
git worktree remove <path>
git worktree prune
```

A worktree carrying uncommitted work is a stop for that branch rather than a
`--force`. Report it, leave both the worktree and the branch, and carry on with
the rest.

**What each branch's pull request was is one more call, not one per branch:**

```sh
gh pr list --state all --limit 200 --json url,state,headRefName
```

Match on `headRefName`. That is where the report gets the URL it prints against
a deleted branch, and a branch matching nothing had no pull request at all.

**The pattern is the guard.** Nothing outside `agent/` is deleted on either side
whatever else a listing turns up, and a branch that is neither `main` nor under
`agent/` is reported and left.

One `push --delete` and one `branch -D` carry every branch.

## What a deletion costs

Both sides go together, so neither is a backup for the other.

- **A branch that had a pull request stays reachable through it.** GitHub keeps
  `refs/pull/<n>/head` after the branch is gone, so
  `git fetch origin refs/pull/<n>/head` brings the commits back. Measured
  2026-08-05 against a pull request whose branch `delete_branch_on_merge` had
  already deleted.
- **A branch that never had one leaves the hash, and sometimes the objects.**
  Where the local side existed, git holds the unreachable commit for
  `gc.pruneExpire`, two weeks by default, so the hash still resolves here and
  `git fsck --unreachable` finds it where none was written down. A branch that
  was only ever on the remote leaves the hash and nothing local to resolve it
  against, which is why the report prints it.

## Report

One line per branch. No narration.

```
deleted  agent/quiet-kestrel/wip   b31f0c4  both    closed unmerged  <pull request URL>
deleted  agent/slate-marten/spike  a01f3c9  local   never pushed
deleted  agent/copper-otter/nas    8f21a04  remote  no pull request
```

**Which sides existed is a column**, since a branch on one side only is the
ordinary case: a local one was never pushed, and a remote one outlived a
checkout somebody deleted.

**A pull request is named by URL rather than by number**, so the thing the user
has to go and look at is one click rather than one search. The URL goes last on
the line, where its length costs the columns nothing.

A run that stopped says so in place of the list and names every open pull
request by URL, with whether it is a draft, since a draft is the likelier one to
have been forgotten.

## The retry is another invocation

A stopped run comes back only when the user asks again. Do not wait for the pull
request, do not poll, and do not offer to carry on afterwards. Merging it,
closing it, or going on working is the decision the stop exists to hand back,
and a run that resumed on its own would land in the middle of it.
