# CLAUDE.md

- @CLAUDE.CUSTOM.md
- @AGENTS.md

Claude Code's own tools and syntax: how it carries out the rules
`CONTRIBUTING.md` and `AGENTS.md` set. Another harness takes its rules from
those two and can skip this file. This one is identical in every repository the
owner keeps; `CLAUDE.CUSTOM.md` is where this one's own work goes.

## Waiting on something long

- Start it with `run_in_background: true` and wait for the completion
  notification. That is the one-minute backgrounding threshold in `AGENTS.md`.
- Foreground `sleep` is blocked, and chaining shorter sleeps does not get around
  it. Polling a background command is wasted, since its completion re-invokes
  you.
- Read the log file for interim output. That needs no wait.
- Wait on a condition with `Monitor` and an until-loop.
- **A completion notification carries the whole command's exit code**, which in
  a logged run is the trailing echo's. Read the real one out of the log.

## The trailers

`AGENTS.md` requires `Co-Authored-By:` on every commit and no co-author trailer
in the body. This harness adds a second one, the session link:

```
Claude-Session: https://claude.ai/code/session_<id>
```

**It goes in the pull request body.** The squash keeps the body, harvests the
co-authors, and discards every other trailer. This harness's standing
instruction is to end every commit message with both, which fires at
`git commit` -- before the body is written by a different command -- so a run
that follows it and stops there writes the trailer twice and lands it zero
times.

**Take the `session_...` web identifier, not the local session UUID** under
`~/.claude/projects`. Different keyspace, and the obvious guess produces a link
that resolves to nothing.

**A session that is not remote controllable has no web identifier.** Leave the
trailer off rather than substitute the UUID.

**The link is live rather than a citation.** Opening it returns to the session
that produced the change, from any machine, and carries on talking to it. That
is what earns it the width. It opens for the account that created it and costs
every other reader nothing to ignore.

## Getting a message in

A long `-m` loses newlines. A heredoc gets the message in as written:

```
git commit -m "$(cat <<'EOF'
Subject line

Body...
EOF
)"
```

`gh pr create --title ... --body "$(cat <<'EOF' ... EOF )"` is the same shape.
Reach for `git commit -F` or `gh pr create --body-file` only where the size
makes inline input impractical.

## Skills leave nothing uncommitted

`AGENTS.md` has the rule that a session commits what it writes. A skill branches
off `main` and names the branch before writing anything, rather than landing its
output on whatever happened to be checked out.

## Asking about a finding

`AskUserQuestion` is what the cold read in `AGENTS.md` uses for an omission or a
misplacement: one question at a time, a preview showing the text that would
land, and a "nothing" option that is meant.

## Handing a privileged command to the user

Print the command and ask for the output. Do not say how to run it.

**Never suggest `!` for one.** `sudo` authenticating through `pam_pkcs11`
segfaults rather than prompting, the prompt having to appear inside the tool's
own process where nobody is watching.

**`CLAUDE_CODE_REMOTE=true` is this harness's disposable-guest signal.** A cloud
session's VM carries it and a local session never does, per Anthropic's
"Configure cloud environments", read 2026-08-11 at
<https://code.claude.com/docs/en/cloud-environments>.

## A pasted image is already a file

Claude Code reads a pasted image off the system clipboard and writes it to
`~/.claude/image-cache/<session-id>/`, numbered in paste order, where it stays
after the session ends. A screenshot of a bank statement, a document, or a
console showing credentials is on disk the moment it is pasted.

Remind the user to clear that directory when a session involved anything
sensitive.
