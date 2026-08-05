# AGENTS.md

Agent rules for this repository. Every rule here is one any agent can carry out,
wherever it is running, and this file reads the same in every repository the
owner keeps.

Read `CONTRIBUTING.md` first. It has this repository's commands, what each does,
and the gate a change has to pass.

**Then read `AGENTS.CUSTOM.md`, which is this repository's own and overrides
anything here it disagrees with.** It is imported below, and a harness that does
not expand an import has to open it.

- @AGENTS.CUSTOM.md

## Scope Of These Files

- `CONTRIBUTING.md`: this repository's commands, prerequisites and gate, and the
  rough edges in its tooling. It is a lookup rather than a process, and it
  belongs to this repository alone.
- `AGENTS.md`: the working process and every agent rule that holds anywhere. It
  is identical in every repository, so a change to it is a change to all of
  them.
- `AGENTS.CUSTOM.md`: the agent rules that are this repository's alone.
- A file named for a particular harness, and the `CUSTOM` counterpart beside it:
  that tool's own syntax and mechanics, split the same way. What is in one
  belongs to the harness it is named for. **Another harness reading it can skip
  it**, and nothing in it is a rule anyone else has to translate.
- If guidance changes behavior, write it in whichever file owns it, and say it
  once.
- **Where a rule would name this repository, it belongs in a `CUSTOM` file.**
  The shared files carry no hostname, no account name, and no repository name,
  which is what lets them be copied rather than reconciled.

## How A Change Gets Made

Work on a branch, open a pull request, and let it be reviewed. That is the whole
of it.

An agent's branch is `agent/<slug>/<what>`. **The slug identifies rather than
describes**: two arbitrary words, `amber-heron` or `slate-marten`, with what the
branch is for going in `<what>`. A slug naming the work defeats its own purpose,
because the next agent on that subject coins the same one -- and then one prefix
holds two agents' branches, so neither whose branch this is nor what one agent
made has an answer any more.

**One slug per agent, a nested one included**, checked with
`git ls-remote --heads origin 'agent/<slug>/*'` before first use. Everything one
agent creates lives under it: the branch its pull request opens from, a second
attempt, a backup taken before a rewrite. That makes an agent's whole output one
prefix to list and one prefix to sweep, and it makes whose branch this is a
question the name answers.

Taking over somebody else's pull request means pushing to their branch, which
belongs to the pull request rather than to a session. What a slug governs is
what an agent creates.

**Open a draft pull request when the work starts**, not when it finishes, and
push to it freely. The branch is where the journey lives -- half-finished
attempts, a commit that says "try the other thing", the detour that got
reverted. None of it reaches `main`, because the merge squashes, and the closed
pull request keeps it afterward.

**One pull request, one concern.** Two orthogonal changes in one make the squash
commit describe two things and the review two reviews. Work that turns out to be
several concerns becomes a stack, each pull request based on the branch below
it, and merging a layer lands every unmerged layer under it.

**Maintenance of this repository's own meta is one concern.** Guidance, skills,
documentation, and whatever the repository uses to track its own work batch
together into a single pull request however many files they touch, and however
unrelated they look. The list is what meta happens to mean here rather than a
closed set, so a repository keeping a kind of its own has it covered too.
Nothing bisects to a prose edit, so the property the rule protects is not there
to protect, and a pull request per edit costs more attention than it returns.
The condition is that the body lists every change in it, since a batch is only
legible if something enumerates it.

One concern per pull request is the whole of history curation. Commits inside a
branch are working notes, so commit as often as is useful and leave them untidy.
There is no rebasing to do and no history to rewrite.

**Draft means working; ready means done.** A draft is open to review the whole
time, so comment on one to steer it. Taking it out of draft is the author's
claim that the work is finished, and the claim comes with a gate: read the
cumulative diff cold, and the gate `CONTRIBUTING.md` names green. Converting
back to draft is the right answer to a review that says it is not close.

### The pull request message

**The pull request title and body are the commit message.** The squash is what
lands, so the body carries the reasoning for the change and the evidence behind
any claim it makes, exactly as a commit message would. Write the subject as a
plain sentence -- a capital letter, no full stop, no category prefix like
`docs:` -- which is the form a branch commit's subject takes as well.

**The body describes the change, not the process that produced it.** No citing
the rule that allowed a batch, no noting which convention is being followed. A
reader on `main` in two years wants the change; spending the first sentence on
the workflow spends the sentence that decides whether they read the rest.

**Keep the body current as the branch changes.** It describes what will land
rather than what was intended when the work started, so a branch that gains a
concern or revises a claim has its body updated in the same round. Leaving it to
the end is how it gets forgotten, and a reviewer reads it to know what they are
looking at. Verification evidence goes stale silently in particular: a run named
in the body has to be the run that covers the last commit.

**Do not hard wrap the body**, which is the one exception to the width
everything else wraps at. A browser reflows it to the width of the window, so
hard-wrapped prose renders jagged against a soft-wrapped edge. Write each
paragraph as one long line and let GitHub break it, which it does when it builds
the squash commit message.

Since the squash discards the branch's own commit messages, a change's story
splits three ways, and each part has one home:

- **The body** is what landed and why -- the commit message someone reading
  `main` will find years later.
- **The thread** is how it got there: what was tried, what a review changed,
  what was ruled out.
- **The branch commits** are working notes and reach nothing.

### Review

**Answer review comments by pushing more commits to the same branch**, and by
replying in the thread. Never a force-push and never a fresh pull request: the
squash means extra commits cost nothing, and a reviewer who cannot see what
changed since their last look has to read all of it again.

**Act on a comment only where its premise holds and you have checked that it
does. Reply on everything else.** A premise verification refutes and a premise
nobody has checked yet are the same case: neither is something to commit
against. Say what you checked and how, or say plainly that you could not check
it, and leave the branch alone until the reviewer has rethought and asked again.

Acting anyway commits a decision they have not made, and it puts the
disagreement in a commit message, where it gets argued with the reviewer instead
of to them. A thread is the cheap place to be wrong; a branch is not.

Recording a measurement is not acting on one. Writing down what a command
returned rests on a premise just verified, and it is often the thing that was
asked for. **The tell is grammatical**: a sentence reporting what was measured
is a measurement, and a sentence turning on "therefore" or "belongs" is a
decision wearing a measurement's clothes.

One held-back item does not hold up the rest of the batch. Everything whose
premise checks out still lands in the same round.

### Stacks

Restacking is the one force-push there is. When a lower layer of a stack gains
commits, the layers above it are rebased onto it and pushed with
`--force-with-lease`, because that is the only way their base moves. It rewrites
nothing a reviewer has already read on the layer itself, which is what separates
it from force-pushing a branch a reviewer has read.

**Restack again once the layer below merges**, and before anything else. The
squash creates a commit that is not an ancestor of the layers above it, so
GitHub retargets their base to `main` and leaves their content untouched --
which offers the merged work a second time, deletions included. Rebase onto the
tip the layer was based on:

```sh
git rebase --onto origin/main <old base tip> <branch>
git push --force-with-lease
```

Nothing warns about this. The pull request reports `mergeable_state: clean`
throughout, and the only symptom is a changed-file count larger than the layer's
own work. Check it after every merge into a stack.

### Merging and cleanup

**The repository owner reviews and merges.** Nothing reaches `main` any other
way. `delete_branch_on_merge` is on, so the remote branch goes with the merge
and what is left is local:

```sh
git checkout main
git pull --ff-only --prune
git branch -D <branch>
```

**The prune is what a plain `git pull` skips**, and the `[gone]` marker below
reads wrong without it: absent until the prune runs, so a merged branch shows as
one that was never pushed.

**`--ff-only` is what keeps a merge commit off `main`.** A local `main` that has
somehow gained a commit of its own makes a plain `git pull` merge rather than
fast-forward, which lands on `main` something no pull request produced, in a
workflow where the squash is meant to be the only way anything gets there.
Refusing is the right failure: it says the tree is in a state worth looking at
rather than quietly resolving it.

A squash merge lands a commit the branch is not an ancestor of, which breaks
every reachability test git has at once. `git branch --merged` never lists the
branch, `git branch -d` refuses it, and `git merge-base --is-ancestor` says no.
**That is why the delete takes a capital.** It is the normal case for every
branch this workflow produces rather than a hazard being overridden on an
unusual one.

It is also why nothing git says settles whether the work landed. The pull
request's own state does, and it does not rest on ancestry:

```sh
gh pr list --head <branch> --state all --json number,state,mergedAt
```

`mergedAt` being set is the whole of the check. The `[gone]` marker that
`git branch -vv` shows against a deleted upstream is a hint rather than proof,
since a remote branch can be deleted without ever having merged.

Then list the prefix, which should come back empty:

```sh
git branch --list 'agent/<slug>/*'
```

**Deleting the branch a pull request opened from is not sweeping the slug.**
Everything one agent creates lives under that prefix -- a second attempt, a
backup taken before a rewrite -- and the steps above all take the branch name as
given, so none of them asks whether the prefix holds anything else. What the
listing turns up is the owner's to keep or discard rather than a sweep's.

### Attribution and provenance

A commit written with an agent carries a `Co-Authored-By:` trailer naming it.
The log says how the repository was written, which is the practice rather than
something to manage.

**The trailer goes on every commit even though the commits are working notes.**
It is what puts the agent's avatar beside each commit in the web interface, and
that rendering comes from the trailer rather than from anything else in the
message.

**The body carries no co-author trailer.** GitHub gathers co-authors from the
squashed commits and appends them under a `---------` rule of its own, so a
`Co-Authored-By:` in the body arrives twice in the landed message. Measured
2026-08-02. It also means the trailer on the commits is what carries
attribution, rather than being the belt to the body's braces.

**A harness may put its own trailer in the body**, and what that trailer is
belongs to the harness rather than to this file. Do not invent one: a trailer
coined locally risks colliding with an official one later, and buys nothing
meanwhile.

## The Cold Read

Leaving draft is gated on reading the cumulative diff cold. A person does that
their own way. This is how an agent does it, since an agent was in the
conversation the whole time and has no instinct to fall back on.

Cold means the conversation is not evidence: anything on the branch was written
by someone who was in it, and the person who reads it in six months sees only
the files.

- **Read every changed file end to end, not the changed sections.** A diff of
  pure additions cannot show a contradiction with a paragraph that was already
  there, and that is where most of them are. A decision made in one section goes
  stale in another the diff never touched, through several reads that covered
  only what had changed.
- Then read what the new text cross-references, and the index entries pointing
  at it.
- A claim that only makes sense because of something said in chat is a finding,
  not a memory to fill in from.

**An inconsistency is a disagreement.** The files contradict each other, or a
file contradicts itself, or the text contradicts the repository's own rules. Fix
it and push the commit. What has actually turned up:

- A section opening with a claim its own later prose exempts something from.
- A table column header that is false for one row, or a column carrying two
  kinds of value so one row reads differently.
- A summary in one file contradicting a reconciliation in another that the same
  session wrote.
- The same fact stated twice in one section, against "say it once".
- An index entry that drifted from what it points at.
- Anything the gate still reports after a formatting run, such as a link to a
  file that was renamed. What a gate covers varies, so a class of defect it
  never checks is one the read has to catch: not every gate resolves an anchor
  against the heading it points at.
- A verbatim quotation whose line breaks the wrap ate.
- Prose that only parses if you already know the superseded version -- "now a
  maintenance bill rather than a saving", or a premise resting on a role
  something was about to take and no longer is. This is the commonest finding
  after a decision changes.
- A backslash the formatter added. Rewrapping can land text at the start of a
  line where it would parse as markup -- `Remote - SSH` broken after "Remote"
  becomes `\- SSH` -- and the escape is correct but reads as damage. Reword so
  the phrase does not sit on the break.
- **A term the change renamed, surviving in prose the diff never touched.**
  Vocabulary drifts one section at a time, so the copy that contradicts is
  usually three sections away from the copy that was fixed.

**An omission is something the files need that never came up.** Do not write it.
Ask, one question at a time, showing the text that would land, and mean the
option to write nothing: most omissions are correctly omitted, and a repository
that answers every question a stranger could ask is padding. Something the
conversation already covered is not an omission -- it was discussed and
deliberately left out, so it stays out.

**A misplacement is live prose that has stopped deciding anything.** Raise it
like an omission, one at a time, since where it moves to is the owner's call and
a section is easier to argue about than to reinstate. It looks like a survey of
options against a choice that is now firm, a dated record whose conclusions are
already stated as decisions elsewhere, a section that says so itself
("conditional on X being reconsidered, which the decision above rules out"), or
anything a reader has to wade through to reach the design. Then watch for the
sections that borrowed the moved one's vocabulary -- they read fine to whoever
moved it and cold to everyone else.

**Any fix earns another read of what it touched**, end to end, plus anything its
new text points at. Files the fix left alone were already read and stay done.
**The read right after your own fix is the one most likely to be fake**: you
wrote that text a minute ago, which puts you further from the stranger it is for
rather than closer, and the finding that catches you is somewhere else in the
file.

Skipping a later read is allowed. Saying you made one is not.

## Writing Style

Prose wraps at 80 columns. Spelling is US English -- `behavior`, `recognized`,
`license`, `optimization` -- and quoted material keeps whatever its source
wrote. Write ASCII: `--` rather than an em dash, `...` rather than an ellipsis,
`->` rather than an arrow, with output from a tool underneath passing through as
it comes. **Which of these a gate enforces varies, so none of them is a rule the
gate is trusted to remember for you**; `CONTRIBUTING.md` says what this
repository checks.

- **Repository prose does not speak in the first person.** No "I", "my", "mine",
  "me", and no "we", "our", or "us" either, in a document, a comment, a guidance
  file, or a pull request body. Name the thing, and where nothing fits, passive
  voice is the lesser evil.
- The reason is attribution rather than register. Repository prose is authored
  by the account that pushed it, so a first-person sentence has the owner
  asserting something in their own voice that a session may have arrived at by
  misreading them, and nobody has the bandwidth to stand behind every sentence
  that lands. Third person leaves the sentence honest about where it came from.
- Chat replies are exempt, and a reply takes whatever voice suits it. So is
  anything the repository is carrying rather than saying: a quotation, and a
  draft of text meant to be published elsewhere. Those are the artifact, and
  rewriting one damages it.
- Start sentences with capital letters and end them with periods, including
  bullet items.
- Keep prose direct and information-dense. Remove filler that does not change
  meaning, and say everything the reader needs.
- **A pull request body is repository prose and a pull request comment is not.**
  The squash makes the body the commit message, so everything here governs what
  it says, and the one exception is how it is wrapped. A comment in the thread
  is conversation and takes the voice a chat reply takes, where warmth is doing
  work rather than padding.
- Say what something is, not what it is not. Contrast earns its place only where
  a reader would otherwise land on the wrong answer.
- Where it earns its place, vary how it is built. A consequence often says more
  than a denial: "device paths move between boots" over "not device paths".
- A phrase that keeps turning up wants varying too, "load-bearing" among them.
  Write "useless" for "vacuous".
- Leave out the defense against an accusation nobody made: "X is not ceremony",
  "not merely tidiness", "the difference is not pedantry". Nothing here is
  decoration, so saying it of one thing implies it of the rest. Open with the
  reason the sentence was already carrying.
- Leave out the restatement that reaches a consequence through its own subject:
  "a lab that needs three secrets remembered is a lab that gets driven by hand".
  Say the consequence.
- Capitals mark a hazard worth stopping at, not a heading. Shout where getting
  it wrong fails silently; write an ordinary sentence everywhere else. What is
  shouted says what a thing is, with any contrast in the prose under it.
- Keep bullet lists flat, and prefer unnumbered lists. Number only when order or
  reference matters.
- Set a verbatim quotation off as a blockquote, or in quotation marks where the
  format has none.
- A table earns its place when the same handful of fields repeat across rows.
- A comment says what the code cannot: the constraint that decides a value, the
  failure a check catches, the upstream behavior being worked around. Code that
  speaks for itself gets none.
- Comments describe the code as it stands. The story of the change -- what was
  tried, what was ruled out, what it used to be -- goes in the pull request
  body.
- Keep a hazard the code prevents, phrased as what the code is for rather than
  as an anecdote. A regression test may name the bug that prompted it, in terms
  that still exist.

## Where Knowledge Goes

- Put durable conventions in these versioned files, not in ephemeral agent
  memory.
- Update guidance when it becomes wrong.
- Say it once in the canonical location.
- **A document describes the plan as it stands.** The story of how it got there
  -- what it used to be, which document promoted a recommendation to a
  requirement, what the first draft said -- goes in the pull request body, which
  is the message that lands and the only one anybody reads later. Rewriting a
  superseded decision as the merits of the current one is the work; a reader
  implementing from a document should not have to carry a settled argument to
  understand what to build.
- Keep what still decides something: a measurement, an upstream behavior, a
  constraint's reason. A dated findings section is a record of what was measured
  and stays as it was written. What goes is the narration of a change of mind.
- A discarded option earns a line where somebody would otherwise propose it
  again, saying what it costs. It does not earn the story of when it lost.
- Leave out the worry that has not happened yet. When something is deferred, say
  so and say what symptom would reopen it, rather than filing it as an open
  question that implies work is owed.
- A guess is labeled as a guess or left out, because an unmarked one reads later
  as settled fact.
- Include a number when it decides something. Leave out numbers that only color
  a sentence.
- Date anything whose meaning depends on when it was written, in ISO form, and
  write absolute dates rather than "last week".
- **A date is often the tell that a sentence has drifted into narration**, and
  the narration is the defect rather than the date. What it narrates is how
  something used to work, or how a draft got to be ready, and that belongs in
  the pull request body or nowhere, depending on whether it will matter
  tomorrow. Cutting the date alone leaves the narration standing, so cut the
  sentence. A measurement that can go stale under you is dated for a good reason
  and keeps its date.
- Thinking out loud is conversation, not a work request. Do not turn it into
  repository prose unasked; when something seems worth writing, offer it in one
  sentence and let the owner choose.

## Research Discipline

- Separate what was measured, what was read, and what was reasoned. A claim that
  blurs the three cannot be rechecked.
- Verify documentary claims from primary sources: the project's own
  documentation, the manual page, the specification, the vendor's page. Quote
  exactly where the wording decides the answer.
- Give the date read for anything from the web, and name the source in the text
  as well as linking it. The name survives a dead link.
- Verify mechanical claims by running something, where running it is cheap. What
  a command actually prints beats reasoning about what it would.
- Label an unverified claim as unverified and say how to verify it.
- Report a search that found nothing as a search that found nothing. Absence of
  a result is a result, and inventing a plausible answer to fill the gap is the
  one unrecoverable failure.
- **Absence from one interface is not absence from the system.** A reference
  listing what an API offers is authoritative about that API and silent about
  the rest, so where a thing looks impossible, ask the schema what the system
  can express. A page that does not mention a mechanism has not ruled it out.
- **Prove it with the smallest disposable artifact.** A throwaway pull request
  settles against a live service in minutes what documentation leaves open.
  Build the thing that can be deleted, and delete it.
- **Hand the owner anything behind a login, and let them find what is stale.**
  The account is theirs, so they are the one who can reach it; the benefit worth
  naming is the second one. Reading one fact out of a vendor portal turns up the
  redesigned site, the enrollment to redo, and the firmware behind on the unit,
  none of which the answer needed and all of which matter. An agent routing
  around a login gets the fact and buries the rest.
- **Ask the owner to read a panel.** What a front-panel display, a label on the
  back of a device, or a switch position says is a measurement no command
  reaches, and they are standing next to it. Ask before writing a physical fact
  off as unreadable.
- The owner's recollection is a prior rather than a source. It is worth testing
  before it is worth arguing with, since checking usually costs one command and
  it decides what to check first. It never makes a claim verified, and a result
  that comes back mixed is reported as mixed.
- A delegated agent's report is evidence, not a source. Check whatever a
  conclusion rests on before it lands in the repository.
- Read the output you got rather than the one you expected. Summarizing a
  command from memory of an earlier run describes a state that may no longer
  exist, and it reads exactly like one that was checked.
- Cite where a reader would otherwise have to take the repository's word for it,
  not once per sentence.

## Writing A Message Git Or GitHub Will Keep

- A session that writes a file commits it. A file left loose gets swept into the
  next unrelated commit, on whatever branch that turns out to be.

- Write a commit message or a pull request body inline rather than through a
  temporary file:

  ```
  git commit -m "$(cat <<'EOF'
  Subject line

  Body...
  EOF
  )"
  ```

  `gh pr create --title ... --body "$(cat <<'EOF' ... EOF )"` is the same shape.
  Use `git commit -F` or `gh pr create --body-file` only when message size makes
  inline input impractical.

- **Re-read git state before acting on it or reporting it.** The owner commits,
  pushes, and edits files while a session is running, so whatever the
  conversation established earlier is a guess by the time it is used.
  `git fetch` and `git status -sb` cost nothing and are the whole check.
  Reporting a branch as four commits ahead is a claim about now, not about when
  it was last looked at.

- That check is not optional before amending, resetting, or rebasing. Restacking
  is the one rewrite this workflow asks for, and the section above bounds it.

## Command Execution

- Detect what the host will allow before branching on it, rather than assuming:
  `sudo -n true`, `ssh-add -l`, network egress. A throwaway sandbox may grant
  passwordless `sudo`, which is a thing to find out rather than to assume either
  way.
- Filtered egress can arrive as a DNS `REFUSED`, so an unreachable mirror reads
  as a name that does not exist.
- Run commands expected to take a minute or more in the background with logged
  output. Say so before running anything long in the foreground.
- Append to logs and timestamp start and end, so a retry preserves the evidence.
- The exit code of `cmd > log; echo "EXIT=$?"` is the echo's. Report the real
  one.
- Prefer readiness checks and pattern waits over `sleep`.
- Use specific process targeting; never broad kills like `pkill -f python`.
- If state is uncertain, re-establish the baseline instead of patching unknowns.

## Privileged Commands

- On a machine the owner keeps, `sudo` is theirs. Hand a privileged operation
  over rather than attempting it.
- In a disposable guest, `sudo` is the agent's.

## Never Write To The Outside World Unasked

- **Never write to the outside world unasked.**
- Pushing an `agent/` branch to this repository, opening the pull request it
  carries, and replying in that pull request's thread are the normal course of
  work and need no separate ask. This file is what authorizes them, and under
  draft-first the push comes at the start of the work rather than at the end.
- Everything else that leaves this machine waits to be asked for: a merge, a
  comment on somebody else's pull request or issue, an issue of one's own, a
  post to a forum, mail, a change to a repository's settings, or any other
  outward write. Report what you would send and let the owner ask for it.
- **A write the owner does ask for carries attribution**, and all of it goes out
  under their account. Where a mechanism carries it the prose does not repeat
  it: a pull request's commits take the `Co-Authored-By:` trailer, which renders
  the agent's avatar beside each one, and the squash carries that into the
  landed message.
- **Everything else says so in its own text, a comment in a pull request thread
  included.** A comment takes no trailer and shows no avatar, so nothing
  whatever separates it from a sentence the owner typed. The same holds for a
  forum post, mail, and an issue. An agent writing under the owner's account is
  indistinguishable from the owner to anything that reads the thread later,
  including another agent, so the sentence saying who wrote it is the whole of
  the difference.
- Reading the outside world is free. Search, fetch, and read as widely as the
  question needs.

## Response Style

- Chat replies have no length target. Match the question: a sentence for a
  sentence, and pages where the research earned them.
- Chat voice can be natural. Repository writing is the disciplined register
  above; a reply is not.
- Compress wording, not substance: do not omit important details.
