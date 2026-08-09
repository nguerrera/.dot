#!/usr/bin/env bash

# Installs the lock-work-area GNOME Shell extension
#
# Copies etc/lock-work-area@local into the user's extension directory and
# enables it. Extensions are copied rather than linked, so this is run by hand
# after a change to the source rather than by deploy.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
UUID=lock-work-area@local
SOURCE_DIR="$SCRIPT_DIR/../etc/$UUID"
DEST_DIR="$HOME/.local/share/gnome-shell/extensions/$UUID"

SCHEMA=org.gnome.shell
KEY=enabled-extensions

if ! which gsettings > /dev/null 2>&1; then
    echo "Error: gsettings not found" >&2
    exit 1
fi

for file in metadata.json extension.js; do
    if [[ ! -f "$SOURCE_DIR/$file" ]]; then
        echo "Error: source file not found: $SOURCE_DIR/$file" >&2
        exit 1
    fi
done

mkdir -p "$DEST_DIR" || exit $?
cp "$SOURCE_DIR/metadata.json" "$SOURCE_DIR/extension.js" "$DEST_DIR/" || exit $?
echo "Installed $UUID to $DEST_DIR."

# The shell notices a new extension directory through a file monitor, so a
# first install can reach here before it knows the uuid, and asking it to
# enable one it has never heard of fails. Writing the uuid into the list the
# shell reads at startup does not depend on that race being won.
ENABLED="$(gsettings get "$SCHEMA" "$KEY")" || exit $?

if echo "$ENABLED" | grep -qF "'$UUID'"; then
    echo "$UUID is already in $SCHEMA $KEY."
else
    # An empty list reads back as "@as []", carrying the type gsettings cannot
    # infer from no elements, and a populated one as "['a@b', 'c@d']".
    LIST="${ENABLED#@as }"

    if [[ "$LIST" == "[]" ]]; then
        LIST="['$UUID']"
    else
        LIST="${LIST%]}, '$UUID']"
    fi

    gsettings set "$SCHEMA" "$KEY" "$LIST" || exit $?
    echo "Added $UUID to $SCHEMA $KEY."
fi

echo "Log out and back in for this to take effect. The shell caches an"
echo "extension's JavaScript once it has imported it, and disabling the"
echo "extension does not reload the file."
