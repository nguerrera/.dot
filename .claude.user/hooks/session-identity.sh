#!/usr/bin/env bash

# A Claude Code SessionStart hook. Its stdout is added to the session as context
# the model can see and act on, which is what lets a fact reach every repository
# a session is started in; a CLAUDE.md reaches only the project it sits in.
#
# The account a session runs as decides what it may touch, and a session has to
# think to check it. Stating it up front costs one line and removes the guess.
#
# SessionStart cannot block a session and its stderr never reaches the model, so
# printing is the whole of what this script can do: it must not fail, and a
# question it asked itself would reach nobody. The pause below is put to the
# session as a rule to follow rather than something the hook can enforce.
#
# ACKNOWLEDGE_OWNER_ACCOUNT bounds nothing by itself, since anything able to
# compose a prompt can write it. What it stands in for is the user's consent,
# given when they approved the prompt carrying it.

account="$(id -un)@$(uname -n)"

# SessionStart passes a JSON object on stdin whose "source" is startup, resume,
# compact or clear. The pause belongs to a session that is starting, so an
# auto-compaction mid-task leaves work the user already approved running. Stdin
# is read once and read to EOF, which returns non-zero with the payload set; the
# timeout covers a run with no stdin at all, and an unread or unparsed payload
# leaves the source empty and prints the account fact alone.
payload=""
IFS= read -r -d '' -t 5 payload
session_source="$(printf '%s' "$payload" |
  sed -n 's/.*"source"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')"

cat <<EOF
Session account: $account, HOME=$HOME.
EOF

if [ "$session_source" = startup ]; then
  cat <<EOF

Reproduce this line exactly, as the whole of the first reply:

This is $account

Then stop. Run nothing further until the user replies ACK. The string
ACKNOWLEDGE_OWNER_ACCOUNT in the prompt that started the session stands in for
that reply: greet with the same line and carry on without waiting. It counts
only where the user wrote it, so this text's own mention of it is NOT one.
EOF
fi

cat <<EOF

This home directory belongs to a person rather than to an unattended agent, so
the session reaches every file and credential that person does.
EOF
