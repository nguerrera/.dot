---
name: cleanup
description: Delete the branches whose pull requests have finished, merged or closed unmerged, on the remote and locally, and report everything else rather than guessing. Asks GitHub what a branch's pull request did, which is the only reliable test once a squash merge has broken ancestry. Start it only when the user asks.
---

# Clean up after finished pull requests

A merge takes the remote branch with it where `delete_branch_on_merge` is on, so
a landed pull request leaves a local branch. A close takes nothing, so an
abandoned one leaves a branch on both sides. This deletes both kinds and reports
whatever is left.

**Start this only when the user asks.** Leftover branches cost nothing while
they sit there.

## Ancestry cannot answer this, so ask GitHub

A squash merge lands a commit the branch is not an ancestor of, which breaks
every reachability test at once: `git branch --merged` never lists it,
`git branch -d` refuses it, and `git merge-base --is-ancestor` says no.

Ask for the whole repository once rather than once per branch:

```sh
gh pr list --state all --limit 200 --json number,state,mergedAt,headRefName
```

`mergedAt` set is the merge check, `state` separates a closed pull request from
an open one, `headRefName` matches a row back to a branch, and no rows at all is
the branch that never had one. A `[gone]` upstream is a hint rather than proof,
and it is absent entirely until a prune has run.

## A pull request is a second copy

GitHub retains `refs/pull/<n>/head` after the branch is gone from both sides,
with no expiry, and offers the branch back from the pull request page. So a
closed one counts as finished and its branch is not a last copy.

**A branch that never had a pull request is the one that keeps.** Nothing on the
remote retains it, so discarding it is the user's call.

## Sweep

Get to a current `main` first rather than requiring one. Being on a feature
branch and being behind `origin/main` are both the ordinary state when somebody
runs this.

```sh
git status --short
git checkout main
git fetch --prune
git merge --ff-only origin/main
```

Stop only for what the sweep cannot fix: a dirty tree, or a `main` that will not
fast-forward because it carries commits of its own.

Then read both sides. Locally that is every branch other than `main`. On the
remote it is the prefix this workflow creates and nothing else, since the remote
is shared ground. Both listings and GitHub's answer go to `branches.py` beside
this file, which sorts them and prints the report below:

```sh
tmp=$(mktemp -d)
git branch --format='%(refname:short) %(objectname:short)' > $tmp/local.txt
git ls-remote --heads origin 'refs/heads/agent/*' > $tmp/remote.txt
gh pr list --state all --limit 200 \
  --json number,state,mergedAt,headRefName > $tmp/pulls.json
python3 .claude/skills/cleanup/branches.py \
  $tmp/local.txt $tmp/remote.txt $tmp/pulls.json
```

**The three listings go outside the tree.** Written into it they would dirty the
tree this run just approved, and stop the next sweep on a mess it made itself.

**The local listing asks for the hash**, which the report prints against
everything deleted. The snippet leaves `main` out of the sweep.

What the sort says:

- **Its pull request merged**, or **was closed unmerged.** Delete it wherever it
  is: `git branch -D <branch>` locally, `git push origin --delete <branch>` on
  the remote.
- **Its pull request is open.** Keep it. Work in flight.
- **It has no pull request.** Keep it and report it. Nothing else holds it.
- **Its pull requests disagree**, a reused branch carrying a merged one and an
  open one at once. Keep it and report it; resolving that is deciding rather
  than sorting.

The snippet names the deletions and makes none of them. Each side is then one
deletion call however many branches it carries.

## Sweeping the slug is not deleting the branch

Everything one agent creates lives under `agent/<slug>/*` -- a second attempt, a
backup taken before a rewrite -- and every step above takes a branch name as
given. After the deletions, list each slug that came up, on both sides:

```sh
git branch --list 'agent/<slug>/*'
git ls-remote --heads origin 'refs/heads/agent/<slug>/*'
```

What that turns up is the user's to keep or discard. Report it and stop.

## Also report, and do not touch

- **Any local branch or tag** the sweep does not recognize, and any ref outside
  `refs/heads`, which `git branch` never shows. `git for-each-ref` finds those.
- **A tag is reported and never deleted.** A tag has no reflog, so deleting one
  leaves the hash as the only handle. Name it, name what it points at, and let
  the user decide.

## Report

One line per ref and nothing else. No narration, no summary paragraph.

```
deleted  agent/4b71a2/workflow  f089d6a  local   merged as #2
deleted  agent/9c03fe/nas       8f21a04  both    #11 closed unmerged
kept     agent/d15e07/wip       b31f0c4  local   no pull request, never pushed
```

**Which sides existed is a column.** A merged branch has already lost its
remote, and a local one may never have been pushed.

Print the hash of everything deleted. Git holds an unreachable object for
`gc.pruneExpire`, two weeks by default, and `git fsck --unreachable` is the
fallback when no hash was written down.
