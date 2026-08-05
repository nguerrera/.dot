---
name: cleanup
description: Delete the local branches whose pull requests have merged, and report everything else rather than guessing. Asks GitHub whether a branch's pull request merged, which is the only reliable test once a squash merge has broken ancestry. Start it only when the user asks.
---

# Clean up after merged pull requests

`delete_branch_on_merge` removes the branch on the remote, so what is left over
is local: a branch per pull request that has landed, plus whatever never landed.
This deletes the first kind and reports the rest.

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

`mergedAt` set is the whole of the check. A `[gone]` upstream is a hint rather
than proof -- a remote branch can be deleted without merging -- and it is absent
entirely until a prune has run, so before that a merged branch reads as one that
was never pushed.

## Sweep

Get to a current `main` first rather than requiring one. Being on a feature
branch and being behind `origin/main` are both the ordinary state at the moment
somebody runs this: the branch that just merged is usually the one checked out,
and the merge that prompted the sweep is why `main` is behind.

```sh
git status --short                 # clean, or stop
git checkout main
git fetch --prune                  # before reading the tracking column
git merge --ff-only origin/main    # refuses a diverged main, which is a stop
git branch --format='%(refname:short) %(upstream:track)'
```

Stop only for what the sweep cannot fix: a dirty tree, or a `main` that
`--ff-only` refuses because it carries commits of its own. The premise of the
sweep is that `main` is what landed, and the fast-forward is how that becomes
true rather than a condition to be met beforehand.

Then, for each local branch other than `main`, ask GitHub and sort:

- **Its pull request merged.** Delete it: `git branch -D <branch>`. The capital
  is the normal case here, per `AGENTS.md`, not a hazard being overridden.
- **Its pull request is open.** Keep it. Work in flight.
- **Its pull request was closed unmerged.** Keep it and report it. That is
  abandoned work whose only copy may be this branch, and discarding it is the
  user's call rather than a sweep's.
- **It has no pull request.** Keep it and report it. Never pushed, so nothing
  else has it.

## Sweeping the slug is not deleting the branch

Deleting the branch a pull request opened from leaves the rest of that agent's
prefix behind. Everything one agent creates lives under `agent/<slug>/*` -- a
second attempt, a backup taken before a rewrite -- and every step above takes a
branch name as given, so none of them asks whether the prefix holds anything
else. After the deletions, list each slug that came up:

```sh
git branch --list 'agent/<slug>/*'
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
deleted  agent/slate-marten/workflow  f089d6a  merged as #2
kept     agent/copper-otter/nas       8f21a04  #11 closed unmerged, only copy
kept     agent/quiet-kestrel/wip      b31f0c4  no pull request, never pushed
```

Print the hash of everything deleted, which makes the transcript the recovery
record. Git keeps an unreachable object for `gc.pruneExpire`, two weeks by
default, and `git fsck --unreachable` is the fallback when no hash was written
down.
