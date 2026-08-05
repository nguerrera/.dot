# CLAUDE.md

- @CLAUDE.CUSTOM.md
- @AGENTS.md

Claude Code's own tools and syntax: how it carries out the rules
`CONTRIBUTING.md` and `AGENTS.md` set. Another harness reaching this file takes
its rules from those two and can skip the rest. This file is identical in every
repository the owner keeps; `CLAUDE.CUSTOM.md` is where this one's own work
goes.

## Waiting on something long

- Start a command in the background with `run_in_background: true`, then wait
  for the completion notification. This is what carries out the one-minute
  backgrounding threshold in `AGENTS.md`.
- Foreground `sleep` is blocked, and chaining shorter sleeps does not get around
  it. Polling a background command is wasted anyway, because its completion
  re-invokes you.
- Read the log file for interim output. That needs no wait.
- Wait on a condition with `Monitor` and an until-loop.
- A completion notification carries the whole command's exit code, which in a
  logged run is the trailing echo's. Read the real one out of the log.

## The trailers

The two `AGENTS.md` requires are ones this harness already emits, so the default
form is the right one. `Claude-Session:` takes the `session_...` web identifier
rather than the local session UUID under `~/.claude/projects`, which is a
different keyspace, so the obvious guess produces a link that resolves to
nothing. `AGENTS.md` has the heredoc that gets a message inline, and says which
trailer goes on a commit and which on a body.

## Skills leave nothing uncommitted

`AGENTS.md` has the rule that a session commits what it writes, and the reason.
What a skill adds is that it branches off `main` and names the branch before
writing anything, rather than landing its output on whatever happened to be
checked out when the user invoked it.

## Asking about a finding

`AskUserQuestion` is what the cold read in `AGENTS.md` uses for an omission or a
misplacement: one question at a time, a preview showing the text that would
land, and a "nothing" option that is meant.

## Handing a privileged command to the user

Prefix it with `!`, which runs it in the session and puts its output in the
conversation. That is the mechanism behind the `sudo` ban.

## A pasted image is already a file

Pasted text lives in the conversation and nowhere else. An image does not:
Claude Code reads it off the system clipboard and writes it to
`~/.claude/image-cache/<session-id>/`, numbered in paste order, and it stays
there after the session ends. So a screenshot of a bank statement, a document, a
console showing credentials, or anything else nobody would type into a file is
on disk the moment it is pasted, without anyone deciding to write it.

Remind the user to clear that directory when a session involved anything
sensitive. The pastes worth keeping are the ones a file already records the
conclusion from, and those do not need the original.
