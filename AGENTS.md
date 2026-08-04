# AGENTS.md

Agent rules for this repository. It holds the configuration files deployed to
the machines that run them -- Arch, Ubuntu, Windows and macOS -- with `deploy`,
`deploy.cmd` and `deploy.ps1` as the entry points.

Read `CONTRIBUTING.md` first. It is the workflow -- branch, draft pull request,
review, squash merge -- and it reads the same for a person and an agent, so an
agent reads it as its own process rather than as background.

## Scope Of These Files

- `CONTRIBUTING.md`: how a change gets made and landed here.
- `AGENTS.md`: what an agent needs told that a person would not. Any agent can
  follow all of it, wherever it is running.
- `CLAUDE.md`: Claude Code's own tools and syntax. What is in there is Claude
  Code's, not a rule another harness has to translate.
- If guidance changes behavior, write it in whichever of the three owns it, and
  say it once.

## Writing Style

- Start sentences with capital letters and end them with periods, including
  bullet items.
- Keep prose direct and information-dense. Remove filler that does not change
  meaning, and say everything the reader needs.
- Say what something is, not what it is not. Contrast earns its place only where
  a reader would otherwise land on the wrong answer.
- Where it earns its place, vary how it is built. A consequence often says more
  than a denial: "device paths move between boots" over "not device paths".
- Leave out the defense against an accusation nobody made: "X is not ceremony",
  "not merely tidiness". Open with the reason the sentence was already carrying.
- Capitals mark a hazard worth stopping at, not a heading. Shout where getting
  it wrong fails silently. What is shouted says what a thing is, with any
  contrast in the prose under it.
- Keep bullet lists flat, and prefer unnumbered lists.
- A comment says what the code cannot: the constraint that decides a value, the
  failure a check catches, the behavior being worked around. Code that speaks
  for itself gets none. Comments describe the code as it stands; the story of
  the change goes in the commit message.
- **Repository prose does not speak in the first person.** No "I", "my", "mine",
  "me", and no "we", "our", or "us" either, in a document, a comment, a guidance
  file, or a pull request body. Name the thing, and where nothing fits, passive
  voice is the lesser evil. The reason is attribution rather than register:
  repository prose is authored by the account that pushed it, so a first-person
  sentence has the owner asserting something in their own voice that a session
  may have arrived at by misreading them. Chat replies are exempt, and so is
  anything the repository is carrying rather than saying, such as a quotation.

## Where Knowledge Goes

- Put durable conventions in these versioned files, not in ephemeral memory.
- Update guidance when it becomes wrong.
- Say it once in the canonical location.
- Include a number when it decides an action. Leave out numbers that only color
  a sentence.
- Date anything whose meaning depends on when it was written, in ISO form, and
  write absolute dates rather than "last week".
- A discarded option earns a line where somebody would otherwise propose it
  again, saying what it costs. It does not earn the story of when it lost.
- Thinking out loud is conversation, not a work request. Do not turn it into
  repository prose unasked; offer it in one sentence and let the user choose.

## Research Discipline

- Separate what was measured, what was read, and what was reasoned. A claim that
  blurs the three cannot be rechecked.
- Verify mechanical claims by running something, where running it is cheap.
- Verify documentary claims from primary sources, and quote exactly where the
  wording decides the answer.
- Give the date read for anything from the web, and name the source in the text
  as well as linking it. The name survives a dead link.
- Label an unverified claim as unverified and say how to verify it.
- Report a search that found nothing as a search that found nothing. Absence of
  a result is a result, and inventing a plausible answer to fill the gap is the
  one unrecoverable failure.
- Read the output you got rather than the one you expected. Summarizing a
  command from memory of an earlier run describes a state that may no longer
  exist, and it reads exactly like one that was checked.
- The user's recollection is a prior rather than a source, worth testing before
  it is worth arguing with. It never makes a claim verified, and a result that
  comes back mixed is reported as mixed.
- A subagent's report is evidence, not a source. Check whatever a conclusion
  rests on before it lands in the repository.

## The Cold Read

`CONTRIBUTING.md` gates leaving draft on reading the cumulative diff cold, and
here that read is the entire gate, since nothing automated exists to catch what
it misses. An agent needs telling how, having been in the conversation the whole
time with no instinct to fall back on.

- **Read every changed file end to end, not the changed sections.** A diff of
  pure additions cannot show a contradiction with a paragraph that was already
  there, and that is where most of them are.
- Then read what the new text cross-references.
- A claim that only makes sense because of something said in chat is a finding,
  not a memory to fill in from.
- **An inconsistency is a disagreement**: the files contradict each other, or a
  file contradicts itself, or the text contradicts the repository's own rules.
  Fix it and push the commit.
- **An omission is something the files need that never came up.** Do not write
  it. Ask, one question at a time, showing the text that would land, and mean
  the option to write nothing.
- **Any fix earns another read of what it touched**, end to end. The read right
  after your own fix is the one most likely to be fake: you wrote that text a
  minute ago, which puts you further from the stranger it is for rather than
  closer.
- Skipping a later read is allowed. Saying you made one is not.

## Writing A Message Git Or GitHub Will Keep

- Write it inline rather than through a temporary file:

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

- **Re-read git state before acting on it or reporting it.** The user commits,
  pushes, and edits files while a session is running, so whatever the
  conversation established earlier is a guess by the time it is used.
  `git fetch` and `git status -sb` cost nothing and are the whole check. That
  check is not optional before amending, resetting, or rebasing.

## Command Execution

- Detect what the host will allow before branching on it, rather than assuming.
  A throwaway sandbox may grant passwordless `sudo`, which is a thing to find
  out rather than to assume either way.
- Run commands expected to take a minute or more in the background with logged
  output. Say so before running anything long in the foreground.
- Append to logs and timestamp start and end, so a retry preserves the evidence.
- The exit code of `cmd > log; echo "EXIT=$?"` is the echo's. Report the real
  one.
- Prefer readiness checks and pattern waits over `sleep`.
- Use specific process targeting; never broad kills like `pkill -f python`.
- **`deploy` writes to a real home directory.** Do not run it to test a change.
  Read it, and let the owner run it.

## Privileged Commands

- On a machine the user owns, `sudo` is theirs. Hand a privileged operation over
  rather than attempting it.
- In a disposable guest, `sudo` is the agent's.

## Never Write To The Outside World Unasked

- Pushing an `agent/` branch to this repository, opening the pull request it
  carries, and replying in that pull request's thread are the normal course of
  work here and need no separate ask.
- Everything else that leaves this machine waits to be asked for: a comment on
  somebody else's pull request or issue, an issue of one's own, a post to a
  forum, mail, a change to a repository's settings, or any other outward write.
  Report what you would send and let the user ask for it.
- Reading the outside world is free. Search, fetch, and read as widely as the
  question needs.
- **A write the user does ask for carries attribution**, and all of it goes out
  under the user's account. The `Co-Authored-By:` trailer carries it for
  commits, and the squash carries that into the landed message.
- **Everything else says so in its own text, a comment in a pull request thread
  included.** A comment takes no trailer and shows no avatar, so nothing
  separates it from a sentence the owner typed. The same holds for a forum post,
  mail, and an issue. An agent writing under the owner's account is
  indistinguishable from the owner to anything that reads the thread later,
  including another agent, so the sentence saying who wrote it is the whole of
  the difference.

## Response Style

- Chat replies have no length target. Match the question.
- Chat voice can be natural. Repository writing is the disciplined register
  above; a reply is not.
