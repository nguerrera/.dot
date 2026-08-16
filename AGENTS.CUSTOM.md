# AGENTS.CUSTOM.md

The agent rules that are this repository's alone.

This repository is a set of configuration files deployed as symlinks from a home
directory on Arch, Ubuntu, Windows and macOS, with `deploy`, `deploy.cmd` and
`deploy.ps1` as the entry points.

## Command Execution

- **`deploy` writes to a real home directory.** Do not run it to test a change.
  Read it, and let the owner run it.

## What `.claude.user` carries

`.claude.user` is deployed as the home directory's `~/.claude`, on the same
terms as `.emacs.d`: the directory is linked whole, and `.gitignore` opts in the
few files worth versioning. `.claude` here is this repository's own skills, so
the name is the only thing the arrangement does differently.

- **The Claude CLI's credentials, transcripts, uploads and pasted images live in
  that directory**, which puts them inside this working tree, untracked.
  **This repository is public**, so `tools/check` enforces the allowlist rather
  than leaving it to `.gitignore` alone. Widening it means both files.
- **What is on the allowlist reaches every session in every repository**:
  `CLAUDE.md` as user instructions, `skills/` as skills offered everywhere,
  `settings.json`, and `hooks/`. Something true of one project belongs in that
  project instead, and a session working elsewhere carries whatever is here into
  a context meant to be clean.
- **Claude Code rewrites `settings.json` as settings change**, so an edit made
  in a session arrives as a modified file. Read what it added before committing
  it; a token or a path worth keeping to oneself does not belong in a file a
  tool writes unprompted.
- `hooks/session-identity.sh` runs at the start of every session in every
  repository, and what it prints is context the model acts on. A claim in it has
  to hold wherever this repository is deployed.
- `deploy` covers this and `deploy.ps1` does not, so a Windows home directory
  gets none of it.
