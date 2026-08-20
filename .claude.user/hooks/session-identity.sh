#!/usr/bin/env bash

# A Claude Code SessionStart hook. Its stdout is added to the session as context
# the model can see and act on, which is what lets a fact reach every repository
# a session is started in; a CLAUDE.md reaches only the project it sits in.
#
# The account a session runs as decides what it may touch, and a session has to
# think to check it. Stating it up front costs one line and removes the guess.
#
# SessionStart cannot block a session and its stderr never reaches the model, so
# nothing here is allowed to fail or to ask a question. The pause below is a
# rule the session follows rather than something a SessionStart hook can
# enforce.

account="$(id -un)@$(uname -n)"

cat <<EOF
Session account: $account, HOME=$HOME.

Reproduce this line exactly, as the whole of the first reply:

Session running as $account.

Then stop. Run nothing further until the user replies ACK.

This home directory belongs to a person rather than to an unattended agent, so
the session reaches every file and credential that person does.
EOF
