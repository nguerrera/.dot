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

`AGENTS.md` requires `Co-Authored-By:` on every commit and no co-author trailer
in the body. The trailer this harness adds is the session link:

```
Claude-Session: https://claude.ai/code/session_<id>
```

**It goes in the pull request body.** `AGENTS.md` has the mechanism: the squash
keeps the body and harvests the co-authors, and discards every other trailer
along with the commit messages. What is this harness's own is that its standing
instruction is to end every commit message with both trailers, which is right
about the co-author and reaches past where the session link survives. That
instruction fires at `git commit`, and the body is written later by a different
command, so a run that follows it and stops there writes the trailer twice and
lands it zero times.

The link takes the `session_...` web identifier rather than the local session
UUID under `~/.claude/projects`, which is a different keyspace, so the obvious
guess produces a link that resolves to nothing.

**A session that is not remote controllable has no web identifier**, and that is
fine: leave the trailer off rather than substitute the UUID. Where a session has
one, both trailers are the default form this harness already emits, so neither
needs looking up.

**The link is live rather than a citation.** Opening it returns to the session
that produced the change, from any machine, and carries on talking to it -- so a
commit on `main` is a way back into the conversation behind it rather than a
record that one happened. That is what earns the width in the message, ahead of
the audit trail it also provides. It opens for the account that created it and
costs every other reader nothing to ignore.

## Getting a message in

`git commit -F` truncates nothing but costs a temporary file, and a long `-m`
loses newlines. A heredoc gets the message in as written:

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
