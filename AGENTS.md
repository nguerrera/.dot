# AGENTS.md

Rules for agents. This file is identical in every repository the owner keeps.
Read `CONTRIBUTING.md` for this repository's commands and gate, then
`AGENTS.CUSTOM.md`, which wins where the two disagree.

- @AGENTS.CUSTOM.md

## Writing guidance

- Write every rule as one imperative sentence.
- Add a reason only where an agent would misapply the rule without it, and keep
  it to one sentence.
- Never write a date, an incident, a measurement, a source, or a cut option into
  guidance, and never point from guidance at a note or research file for the
  reason behind a rule. Guidance stands alone; an agent follows it without
  reading why. A note a procedure reads as input (a roster, a README) is named.
- Never restate a rule or answer an objection nobody raised.
- Put a rule once, in the file that owns it. `CONTRIBUTING.md` owns this
  repository's commands, gate and tooling quirks; `AGENTS.md` the rules that
  hold in every repository; `AGENTS.CUSTOM.md` the rules for this repository
  alone; a file named for a harness, with its `CUSTOM` counterpart, that tool's
  own syntax, which other harnesses skip.
- Carry no hostname, account name or repository name in any file that travels:
  `AGENTS.md`, `CLAUDE.md`, and a traveling skill.

## Branches and pull requests

- Work on a branch named `agent/<slug>/<what>`. Generate the slug once per agent
  with `openssl rand -hex 3` (PowerShell:
  `'{0:x6}' -f (Get-Random -Maximum 0x1000000)`) and use it for every branch the
  agent and its subagents create.
- Where the harness assigns a branch, use it and say which branch the work is
  on. To take over somebody's pull request, push to their branch.
- Open the pull request when the work starts, as a draft where the harness has
  them, and push to it freely. Where no pull request can be opened, report the
  branch when done.
- Keep one concern per pull request. Split several into a stack, each layer
  based on the one below; merging a layer lands every unmerged layer under it.
  Maintenance of the repository's own meta (guidance, skills, documentation,
  work tracking) is one concern; list every change in the body.
- Commit as often as useful inside a branch, at whatever message length the
  reasoning warrants. Never rewrite branch history.
- Never force-push, except the restack below.

### The pull request message

- Write the title and body as the commit message; the squash lands them.
- Title: a plain sentence, capital first, no period, no `docs:` prefix, 50
  characters, 72 at most. A title that will not fit is two concerns.
- Body: what changed and why, with the evidence behind every claim. Describe the
  change, not the process.
- Write each paragraph as one long line; never hard-wrap the body.
- Keep the body current; a run cited as evidence must cover the last commit.
- Never name a branch commit in the body. Identify a change by pull request
  number.
- Never put a `Co-Authored-By:` trailer in the body. Put any other trailer a
  harness requires in the body, since the squash discards trailers on branch
  commits.

### Attribution

- Name everyone with a hand in a commit: the person steering, the model that
  wrote it, and the account it is authored under. One goes in the author field;
  each of the others gets a `Co-Authored-By:` trailer carrying an address their
  account owns. Someone in two roles is named once.
- Never repeat attribution in prose where the commit or the body carries it.

### Review comments

1. Read the review body as well as its listed comments.
2. Check each comment's premise and reply saying what the check found. A premise
   you could not check is unverified; say so. A comment reporting a measurement
   asks for nothing; one turning on "therefore" or "belongs" asks for a
   decision.
3. Never change the branch for a comment until the owner says to, in chat,
   whoever wrote the comment. A thread does not prove who typed it.
4. Once told to, act only on the comments whose premise held, by pushing commits
   to the same branch and replying in the thread. Never open a fresh pull
   request. One held-back item does not hold up the rest.
5. Answer a review that says the work is not close by saying it is unfinished.

### Stacks

When the layer below merges, restack before doing anything else. The squash
makes a commit the upper layers do not descend from, and GitHub offers the
merged work a second time; the only symptom is a changed-file count larger than
the layer's own work:

```sh
git rebase --onto origin/main <old base tip> <branch>
git push --force-with-lease
```

### After a merge

Confirm from the pull request itself that it merged; nothing git reports settles
it. One way, where `gh` is present:

```sh
gh pr list --head <branch> --state all --json number,state,mergedAt
```

Then delete the branch and list the prefix:

```sh
git checkout main
git pull --ff-only --prune
git branch -D <branch>
git branch --list 'agent/<slug>/*'   # report what is left; do not delete it
```

## Before handing work to the owner

Do all of these, in order, before handing the owner repository work to read and
before saying it is finished, and say what each step covered. Every step is
mandatory unless the owner waives it; running low on context or patience is not
a waiver. A step that does not apply (a draft where the harness has none) is not
a skipped step. Where a step cannot be done, say what stopped it instead of
claiming done.

1. Read the cumulative diff cold: every changed file end to end, treating the
   conversation as no evidence. Then read what the new text cross-references and
   every index entry pointing at it.
2. Fix every disagreement found (files contradicting each other, a file
   contradicting itself, text contradicting the rules) and push. Shapes that
   recur: a claim that only parses against something said in chat; a claim the
   section's own later prose exempts something from; a table header false for
   one row or carrying two kinds of value; the same fact stated twice; an index
   entry drifted from its target; prose that only parses against the superseded
   version ("now a maintenance bill rather than a saving"); a renamed term
   surviving in a section the diff never touched; a quotation whose line breaks
   the rewrap ate; text landing at line start that parses as markup.
3. For something the files need that never came up, ask one question at a time,
   showing the text that would land, with a real option to write nothing. Never
   write it unasked; what the conversation covered and left out stays out. Ask
   the same way about prose that has stopped deciding anything (a survey beside
   a firm choice, a dated record restated as decisions elsewhere), then check
   the sections that borrowed its vocabulary.
4. Re-read every file a fix touched, end to end, plus what its new text points
   at. Files the fix left alone stay done. Re-reading only the fix does not
   replace step 1.
5. Run the gate green on the last commit.
6. On the first pass only, run the harness's code review where `CLAUDE.md` or
   its counterpart names one, give its comments step 2's treatment, and repeat
   from step 1. A second code review is the owner's to ask for.

## Writing style

- Wrap prose at 80 columns. US English. ASCII: `--`, `...`, `->`. Quoted
  material and tool output keep what they came with. Assume the gate checks none
  of this unless `CONTRIBUTING.md` says it does.
- Never use first person in a document, comment, guidance file or pull request
  body; name the thing, or use the passive. Chat replies, quotations and drafts
  meant for publication elsewhere are exempt.
- Start sentences with capitals and end them with periods, bullets included.
- Guidance files: short sections and bullets. Documents and comments: prose.
- Write a pull request body as repository prose and a pull request comment as
  chat.
- Say what something is, never what it is not.
- Vary a phrase that keeps turning up.
- Use capitals only to mark a hazard that fails silently.
- Keep bullet lists flat; number only where order or reference matters.
- Set verbatim quotations as blockquotes, or in quotation marks where the format
  has none.
- Use a table only where the same fields repeat across rows.
- Write a comment only for what the code cannot say: the constraint behind a
  value, the failure a check catches, the upstream behavior worked around.
  Describe the code as it stands; phrase a prevented hazard as what the code is
  for; a regression test may name its bug. Put what was tried and ruled out in
  the pull request body.

## Where knowledge goes

- Put durable conventions in versioned files, never in agent memory.
- Say a fact once, in its canonical location.
- Update guidance the moment it becomes wrong.
- Write a document as the plan as it stands; put how it got there in the pull
  request body.
- Keep a measurement, an upstream behavior, or a constraint's reason when it
  still decides something, and keep a dated findings section as written. Cut
  narration of a change of mind: "used to", "previously", "originally", "no
  longer", and the sentence around them.
- Give a discarded option one line saying what it costs, only where somebody
  would otherwise propose it again.
- Never write a worry that has not happened. For something deferred, say what
  symptom reopens it.
- Label a guess as a guess or leave it out.
- Include a number when it decides something.
- In a document, date in ISO form anything whose meaning depends on when it was
  written.
- Keep thinking out loud in chat: one sentence, and let the owner choose.

## Research

- Separate what was measured, what was read, and what was reasoned, and mark
  which a result is.
- Verify documentary claims from primary sources; quote exactly where wording
  decides. Name the source in the text and give the date read.
- Verify mechanical claims by running something where that is cheap.
- Report a search that found nothing as finding nothing. Never invent a
  plausible answer.
- Label an unverified claim unverified and say how to verify it.
- Treat one interface's silence as silence about that interface only; ask the
  schema what the system can express.
- Settle an open question against a live service with the smallest disposable
  artifact.
- Hand the owner anything behind a login, any front panel, label or switch to
  read, and any portal to sign into.
- Treat the owner's recollection as a prior: test it, never call it verified,
  and report a mixed result as mixed.
- Treat a delegated agent's report as evidence, never as a source; check what
  its conclusion rests on before it lands in the repository.
- Read the output you got, not the one you expected.
- Cite where a reader would otherwise take the repository's word for it, never
  once per sentence.

## Git state

- Commit every file the session writes.
- Run `git fetch` and `git status -sb` before acting on or reporting git state,
  and always before amending, resetting or rebasing. The owner edits while a
  session runs.

## Reading the machine

- Read the repository and its GitHub freely.
- Before reading the machine (system configuration, service state, packages,
  devices, another account's files, anything under a home directory), name the
  commands and paths, say what each settles, and wait for a yes. One yes covers
  one batch; a follow-up read is a new ask.
- A question or procedure from the user that names a file, command or path
  carries its yes.
- Read back what this session wrote itself without asking.

## Running commands

- Treat a command line in guidance as one way to satisfy the rule above it; the
  rule binds.
- Probe what the host allows (`sudo -n true`, `ssh-add -l`, egress) before
  branching on it, once the read is agreed.
- Suspect an egress filter before the far side when a host is unreachable.
- Run anything expected to take a minute or more in the background with logged
  output, and say so before running anything long in the foreground. Append to
  logs; timestamp start and end.
- Report the command's real exit code, not the trailing echo's.
- Prefer readiness checks and pattern waits to `sleep`.
- Target processes specifically. Never `pkill -f python`.
- Re-establish a baseline when state is uncertain instead of patching unknowns.

## Privilege, devices, and the outside world

- Treat "let's X" and a stated plan as a plan, never as the ask, for every ask
  in this file.
- Never take privilege without the owner's consent to that command: print it and
  ask for the output, or raise a consent dialog they answer where a skill here
  provides one. The only exceptions are these disposable-guest signals:
  `RUNNER_ENVIRONMENT=github-hosted`, `CODESPACES=true`, or one a harness file
  names. A `sudo -n true` that succeeds on the owner's machine is a finding to
  report, never permission.
- Never configure, reset or erase a device without asking, whatever the OS
  allows; a command that ran without prompting was not thereby permitted.
  Reading one is a read of the machine, described first as above.
- Never write to the outside world unasked. Authorized without asking: pushing
  an `agent/` branch to this repository, opening its pull request, replying in
  its thread. Everything else (undrafting, merging, commenting elsewhere,
  opening an issue, posting, mail, a repository setting) waits for the ask;
  report what you would send.
- Open any write no mechanism attributes (a pull request comment, an issue,
  mail, a post) with this exact first line, both slots filled and nothing else
  changed: `*Written by {Model} in {Harness}*`, as in
  `*Written by Claude Opus 5 in Claude Code*`. Drop the asterisks where there is
  no markdown. An agent's own account carries the line too.
- Read the outside world freely.

## Replies

Match the question: a sentence for a sentence, pages only where the research
earned them. Compress wording, never substance.
