# AGENTS.CUSTOM.md

The agent rules that are this repository's alone.

It is a set of configuration files deployed as symlinks from a home directory on
Arch, Ubuntu, Windows and macOS, with `deploy`, `deploy.cmd` and `deploy.ps1` as
the entry points.

## Command Execution

- **`deploy` writes to a real home directory.** Do not run it to test a change.
  Read it, and let the owner run it.
