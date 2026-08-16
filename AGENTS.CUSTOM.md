# AGENTS.CUSTOM.md

The agent rules that are this repository's alone.

This repository is a set of configuration files deployed as symlinks from a home
directory on Arch, Ubuntu, Windows and macOS, with `deploy`, `deploy.cmd` and
`deploy.ps1` as the entry points.

## Command Execution

- **`deploy` writes to a real home directory.** Do not run it to test a change.
  Read it, and let the owner run it.

## What `etc/claude` carries

`etc/claude` is deployed into `~/.claude`, which holds the Claude CLI's own
credentials and transcripts and is skipped as a whole by the loop that links
everything else.

- **Claude Code rewrites `settings.json` as settings change**, so an edit made
  in a session arrives here as a modified file. Read what it added before
  committing it: **this repository is public**, and a token, a credential or a
  path worth keeping to oneself does not belong in a file a tool writes
  unprompted.
- `hooks/session-identity.sh` runs at the start of every session in every
  repository, and what it prints is context the model acts on. A claim in it has
  to hold wherever this repository is deployed.
- `deploy` covers this and `deploy.ps1` does not, so a Windows home directory
  gets none of it.
