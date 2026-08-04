# CONTRIBUTING.md

How a change gets made and landed here. It reads the same for a person and an
agent.

## The Workflow

Work on a branch, open a pull request, let it be reviewed. That is the whole of
it.

`tools/check` is the thing to run, and it needs nothing installed. It reads
every tracked `.md` file for a line over 80 columns, a non-ASCII character, and
a relative link that does not resolve. CI runs it on every push and every pull
request.

- **An agent's branch is `agent/<slug>/<what>`**: two ordinary words naming the
  work, then what this particular branch is. The slug names the work rather than
  the session, so it survives a handoff. Run
  `git ls-remote --heads origin 'agent/<slug>/*'` before first use, which makes
  it collision-free rather than merely unlikely.
- **Open a draft pull request when the work starts**, not when it finishes, and
  push to it freely. The branch is where the journey lives, and none of it
  reaches `main` because the merge squashes.
- **One pull request, one concern.** Two unrelated changes in one make the
  squash commit describe two things and the review two reviews.
- **Maintenance of this repository's own meta is one concern.** Guidance and
  documentation batch into a single pull request however many files they touch,
  on the condition that the body lists every change in it.
- Commits inside a branch are working notes. Commit as often as is useful and
  leave them untidy; there is no history to curate below the squash.
- **Draft means working; ready means done.** Taking a pull request out of draft
  is the author's claim that the work is finished. **The gate is reading the
  cumulative diff cold, and `tools/check` green.** Converting back to draft is
  the right answer to a review that says it is not close.

## The Pull Request Message

- **The pull request title and body are the commit message**, because the squash
  is what lands. Write the subject as a plain sentence: a capital letter, no
  full stop, no category prefix like `docs:`.
- **The body describes the change.** The rule that allowed a batch and the
  convention being followed are process, and the reader came for the change.
- **Keep the body current as the branch changes.** It describes what will land
  rather than what was intended when the work started.
- **Do not hard wrap the body**, which is the one exception to the 80 columns
  markdown wraps at here. A browser reflows it, so hard-wrapped prose renders
  jagged. Write each paragraph as one long line.
- The story splits three ways with one home each: **the body** is what landed
  and why, **the thread** is how it got there, and **the branch commits** are
  working notes that reach nothing.

## Review

- **Answer review comments by pushing more commits to the same branch**, and by
  replying in the thread. Never a force-push and never a fresh pull request.
- **Act on a comment only where its premise holds and you have checked that it
  does. Reply on everything else.** Acting anyway commits a decision the
  reviewer has not made and puts the disagreement in a commit message. Recording
  a measurement is not acting on one, and the tell is grammatical: a sentence
  reporting what was measured is a measurement, a sentence turning on
  "therefore" is a decision wearing a measurement's clothes.
- **Restacking is the one force-push there is**, for a stack whose lower layer
  gained commits, with `--force-with-lease`. Restack again once the layer below
  merges and before anything else: the squash creates a commit the upper layer
  is not a descendant of, so it will offer the merged work a second time,
  deletions included, while reporting `mergeable_state: clean` throughout.

## Merging And Cleanup

The repository owner reviews and merges. `delete_branch_on_merge` is on, so the
remote branch goes with the merge and what is left to clean up is local:
`git checkout main`, pull, and delete the branch with `git branch -D`.

A squash merge lands a commit the branch is not an ancestor of, which breaks
every reachability test git has at once. `git branch --merged` never lists the
branch, `git branch -d` refuses it, and `git merge-base --is-ancestor` says no.

That is why the delete takes a capital, on every branch this workflow produces
rather than on an unusual one where a hazard is being overridden.

It is also why nothing git says settles whether the work landed. The pull
request's own state does, and it does not rest on ancestry:

```sh
gh pr list --head <branch> --state all --json number,state,mergedAt
```

`mergedAt` set is the whole of the check. The `[gone]` marker that
`git branch -vv` shows against a deleted upstream is a hint rather than proof,
since a remote branch can be deleted without ever having merged.

## Attribution

- A commit written with an agent carries a `Co-Authored-By:` trailer naming it,
  on every commit even though the commits are working notes. That trailer is
  what puts the agent's avatar beside each commit in the web interface.
- **The pull request body carries a session link and no co-author trailer**:

  ```
  Claude-Session: https://claude.ai/code/session_<id>
  ```

  GitHub gathers co-authors from the squashed commits and appends them under a
  rule of its own, so a `Co-Authored-By:` written into the body arrives twice in
  the landed message. The trailer on the commits is what carries attribution.

- **The session link is live, which is why it is worth the width.** Opening it
  returns to the session that produced the change, from any machine, and
  continues it, so a commit is a way back into the conversation behind it rather
  than only a record that one happened. It resolves for the account that created
  it, and costs every other reader nothing to ignore.

## Writing

- Markdown prose wraps at 80 columns, **by hand, since nothing here reformats
  it**. `tools/check` is what catches a line that missed the wrap.
- Spelling is US English: `behavior`, `recognized`, `license`, `optimization`.
  Quoted material keeps whatever its source wrote.
- Write ASCII: `--` rather than an em dash, `...` rather than an ellipsis, `->`
  rather than an arrow. Output from a tool underneath passes through as it
  comes.
