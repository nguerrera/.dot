---
name: cleanup
description: Delete the branches whose pull requests have finished, merged or closed unmerged, on the remote and locally, and report everything else rather than guessing. Asks GitHub what a branch's pull request did, which is the only reliable test once a squash merge has broken ancestry. Start it only when the user asks.
---

# Clean up after finished pull requests

Start this only when the user asks. It deletes the branches whose pull request
merged or closed, on both sides, and reports everything else.

## Ask GitHub, never ancestry

A squash merge lands a commit the branch is not an ancestor of, so
`git branch --merged`, `git branch -d` and `git merge-base --is-ancestor` all
answer wrong. A `[gone]` upstream is a hint, absent until a prune has run.

Ask once for the whole repository:

```sh
gh pr list --state all --limit 200 --json number,state,mergedAt,headRefName
```

`mergedAt` set is merged; `state` separates closed from open; `headRefName`
matches a row to a branch; no row is a branch that never had a pull request.

GitHub retains `refs/pull/<n>/head` after both branches are gone, so a closed
pull request's branch is not a last copy. A branch that never had a pull request
is the one that keeps.

## Sweep

Get to a current `main` first:

```sh
git status --short
git checkout main
git fetch --prune
git merge --ff-only origin/main
```

Stop only for a dirty tree or a `main` that will not fast-forward.

Then list both sides for the agent prefixes (`agent/*`, `copilot/*`,
`claude/*`), ask GitHub, and hand all three to `branches.py` beside this file,
which sorts and prints the report:

```sh
tmp=$(mktemp -d)
git branch --list 'agent/*' 'copilot/*' 'claude/*' \
  --format='%(refname:short) %(objectname:short)' > $tmp/local.txt
git ls-remote --heads origin 'refs/heads/agent/*' 'refs/heads/copilot/*' \
  'refs/heads/claude/*' > $tmp/remote.txt
gh pr list --state all --limit 200 \
  --json number,state,mergedAt,headRefName > $tmp/pulls.json
python3 .claude/skills/cleanup/branches.py \
  $tmp/local.txt $tmp/remote.txt $tmp/pulls.json
```

- Write the listings outside the tree.
- Read both sides with the same three patterns, or the sides column lies.
- Treat every prefix alike; a finished pull request is finished whichever
  carries it.

The sort:

- Pull request merged or closed unmerged: delete it wherever it is,
  `git branch -D <branch>` locally and `git push origin --delete <branch>`
  remotely.
- Pull request open: keep it.
- No pull request: keep it and report it.
- Pull requests disagree (a reused branch with a merged and an open one): keep
  it and report it.

The snippet names the deletions and makes none.

## Then list the slug

After the deletions, list each `agent/<slug>` that came up, both sides, and
report what is left without deleting it:

```sh
git branch --list 'agent/<slug>/*'
git ls-remote --heads origin 'refs/heads/agent/<slug>/*'
```

`copilot/*` and `claude/*` have no slug to list.

## Also report, and do not touch

```sh
git for-each-ref --format='%(refname) %(objectname:short)' \
  | grep -Ev '^refs/(remotes|heads/(main|agent|copilot|claude))[ /]'
```

Report a local branch outside the three prefixes under its full `refs/heads/`
name. Report a tag with what it points at and never delete it.

## Report

One line per ref, nothing else:

```
deleted  agent/4b71a2/workflow   f089d6a  local   merged as #2
deleted  agent/9c03fe/nas        8f21a04  both    #11 closed unmerged
deleted  copilot/add-a-fixture   3d7be10  remote  merged as #14
kept     agent/d15e07/wip        b31f0c4  local   no pull request, never pushed
kept     refs/heads/scratch      425044d          outside the swept prefixes
kept     refs/tags/v1            9a1c07b          tag, points at main
```

Print the hash of everything deleted; `git fsck --unreachable` is the fallback
where none was written down, within `gc.pruneExpire`. A ref the patterns did not
reach keeps its full name and no sides.
