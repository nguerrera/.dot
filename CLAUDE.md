# CLAUDE.md

- @CLAUDE.CUSTOM.md
- @AGENTS.md

How Claude Code carries out `AGENTS.md` with its own tools. This file is
identical in every repository the owner keeps; `CLAUDE.CUSTOM.md` is this
repository's own.

## Waiting on something long

- Start anything that may run a minute or more with `run_in_background: true`
  and wait for the completion notification. Never poll it, and never chain
  `sleep`s; foreground `sleep` is blocked.
- Read the log file for interim output.
- Wait on a condition with `Monitor` and an until-loop.
- Read the real exit code out of the log; the completion notification carries
  the trailing echo's.

## The trailers

- End every commit message with the `Co-Authored-By:` trailers `AGENTS.md`
  requires, one per party not in the author field.
- Put the session link in the pull request body, never in a commit, in this
  form: `Claude-Session: https://claude.ai/code/session_<id>`. Use the
  `session_...` web identifier, never the local UUID under `~/.claude/projects`.
  Where the session has no web identifier, leave the trailer off.

## Getting a message in

Pass a multi-line commit or pull request message through a quoted heredoc, since
`-m` with a long string loses newlines:

```
git commit -m "$(cat <<'EOF'
Subject line

Body...
EOF
)"
```

`gh pr create --body "$(cat <<'EOF' ... EOF )"` takes the same shape. Use
`git commit -F` or `--body-file` only where the size makes inline input
impractical.

## Asking about a finding

Use `AskUserQuestion` for the question the `AGENTS.md` checklist asks about an
omission, with the text that would land in the option preview.

## The code review

`/code-review` is the harness's code review named in `AGENTS.md`'s checklist;
treat what it returns as review comments.

## Handing a privileged command to the user

- Never say how to run it.
- Never suggest the `!` prefix for a privileged command. It runs in the tool's
  own process, where `sudo` has no terminal to authenticate on, and wrapping it
  in a pty puts a prompt for the owner's credential in front of the session.
- `CLAUDE_CODE_REMOTE=true` is this harness's disposable-guest signal, per
  `AGENTS.md`. A cloud session's VM carries it; a local session never does.

## A pasted image is already a file

Claude Code writes every pasted image to `~/.claude/image-cache/<session-id>/`,
where it stays after the session ends. Remind the user to clear that directory
when a session involved anything sensitive.
