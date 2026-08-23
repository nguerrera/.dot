# CLAUDE.md

User instructions for every Claude Code session started under this account, in
every repository. This home directory belongs to a person rather than to an
unattended agent, so a session reaches every file, credential and device that
person does, and nothing in the harness stops it once it is moving.

- Where a repository's `AGENTS.md` and this file disagree, this file wins.
- The sections below are a verbatim copy of the `AGENTS.md` sections of the
  same names, so a repository with no `AGENTS.md` gets the same rules. Read
  "this repository" in them as the repository the session is in, and "a skill
  here" as a skill the session has.

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
  mail, a post) with a line naming whatever of the model and the harness the
  reader cannot see already, then a blank line:
  `*Written by {Model} in {Harness}*`, as in
  `*Written by Claude Opus 5 in Claude Code*`; `*Written by {Name}*` where the
  two share a name; nothing where the posting account's name says both. Drop the
  asterisks where there is no markdown.
- Read the outside world freely.
