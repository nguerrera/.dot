# CLAUDE.md

- @AGENTS.md

Claude Code's own tools and syntax: how it carries out the rules
`CONTRIBUTING.md` and `AGENTS.md` set. Another harness reaching this file takes
its rules from those two and can skip the rest.

## Waiting On Something Long

- Start a command in the background with `run_in_background: true`, then wait
  for the completion notification. Foreground `sleep` is blocked, and chaining
  shorter sleeps does not get around it.
- Read the log file for interim output. That needs no wait.
- A completion notification carries the whole command's exit code, which in a
  logged run is the trailing echo's. Read the real one out of the log.

## Handing A Privileged Command To The User

Prefix it with `!`, which runs it in the session and puts its output in the
conversation.

## The Trailers

Both are ones this harness already emits, so the default form is the right one.
`Claude-Session:` takes the `session_...` web identifier rather than the local
session UUID under `~/.claude/projects`, which is a different keyspace and
resolves to nothing, so the obvious guess produces a link pointing at nothing.
`CONTRIBUTING.md` says which trailer goes on a commit and which on a body.
