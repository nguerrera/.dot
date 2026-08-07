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
they sit there, so there is no reason to reach for this unprompted.

## Ancestry cannot answer this, so ask GitHub

A squash merge lands a new commit that the branch is not an ancestor of, which
breaks every reachability test at once: `git branch --merged` never lists it,
`git branch -d` refuses it, and `git merge-base --is-ancestor` says no. All
three are answering a question about history that squashing made unanswerable.

The authoritative test is the pull request's own state:

```sh
gh pr list --head <branch> --state all --json number,state,mergedAt
```

`mergedAt` set is the whole of the merge check, `state` separates a closed pull
request from an open one, and no rows at all is the branch that never had one. A
`[gone]` upstream is a hint rather than proof -- a remote branch can be deleted
without merging -- and it is absent entirely until a prune has run, so before
that a merged branch reads as one that was never pushed.

## A pull request is a second copy

A branch whose pull request exists is not the only copy of anything, whatever
that pull request did. GitHub retains `refs/pull/<n>/head` after the branch is
gone from both sides, and offers the branch back from the pull request itself:

> You can restore the head branch of a closed pull request.

From "Deleting and restoring branches in a pull request" on `docs.github.com`,
read 2026-08-05, which sets no expiry against it. Measured the same day against
a pull request closed unmerged whose branch had just been deleted on both sides:
`git ls-remote origin 'refs/pull/<n>/*'` still listed the head, and
`git fetch origin refs/pull/<n>/head` returned the commit.

**A branch that never had a pull request is the one that keeps.** Nothing on the
remote retains it, so the branch is the only handle on that work and discarding
it is the user's call rather than a sweep's.

## Sweep

Get to a current `main` first rather than requiring one. Being on a feature
branch and being behind `origin/main` are both the ordinary state at the moment
somebody runs this: the branch that just merged is usually the one checked out,
and the merge that prompted the sweep is why `main` is behind. Four things, in
order: the tree is clean or the run stops, `main` is what is checked out, the
remote's view is refreshed with deleted upstreams pruned from it before anything
reads a tracking column, and `main` is fast-forwarded to what the remote has.

```sh
git status --short
git checkout main
git fetch --prune
git merge --ff-only origin/main
```

Stop only for what the sweep cannot fix: a dirty tree, or a `main` that will not
fast-forward because it carries commits of its own. The premise of the sweep is
that `main` is what landed, and the fast-forward is how that becomes true rather
than a condition to be met beforehand.

Then read both sides. Locally that is every branch other than `main`. On the
remote it is the prefix this workflow creates and nothing else, since the remote
is shared ground and a sweep has no business deleting what it did not put there:

```sh
git branch --format='%(refname:short) %(upstream:track)'
git ls-remote --heads origin 'refs/heads/agent/*'
```

Ask GitHub about each name that came up, and sort:

- **Its pull request merged.** Delete it wherever it is: locally with
  `git branch -D <branch>`, and on the remote with
  `git push origin --delete <branch>` where it is still carried there. Forcing
  the local delete is the normal case here, per `AGENTS.md`, rather than a
  hazard being overridden.
- **Its pull request was closed unmerged.** Delete it the same way, on the
  ground above.
- **Its pull request is open.** Keep it. Work in flight.
- **It has no pull request.** Keep it and report it. Never pushed, or pushed and
  never opened, so nothing else holds it.

Each side is one deletion call however many branches it carries.

## Sweeping the slug is not deleting the branch

Deleting the branch a pull request opened from leaves the rest of that agent's
prefix behind. Everything one agent creates lives under `agent/<slug>/*` -- a
second attempt, a backup taken before a rewrite -- and every step above takes a
branch name as given, so none of them asks whether the prefix holds anything
else. After the deletions, list each slug that came up, on both sides:

```sh
git branch --list 'agent/<slug>/*'
git ls-remote --heads origin 'refs/heads/agent/<slug>/*'
```

What that turns up is the user's to keep or discard rather than a sweep's, so
report it and stop.

## Also report, and do not touch

- **Any local branch or tag** the sweep does not recognize, and any ref outside
  `refs/heads`, which `git branch` never shows. `git for-each-ref` is what finds
  the second kind.
- **A tag is reported and never deleted.** A tag has no reflog, so deleting one
  leaves the hash as the only handle, where a deleted branch can still be
  recovered from where it pointed. Name it, name what it points at, and let the
  user decide.

## Report

One line per ref and nothing else. No narration, no summary paragraph.

```
deleted  agent/4b71a2/workflow  f089d6a  local   merged as #2
deleted  agent/9c03fe/nas       8f21a04  both    #11 closed unmerged
kept     agent/d15e07/wip       b31f0c4  local   no pull request, never pushed
```

**Which sides existed is a column**, since a branch on one side only is the
ordinary case: a merged one has already lost its remote, and a local one may
never have been pushed.

Print the hash of everything deleted. The pull request is the recovery path for
anything that had one, and the hash is what makes the transcript one as well:
git holds an unreachable object for `gc.pruneExpire`, two weeks by default, and
`git fsck --unreachable` is the fallback when no hash was written down.
