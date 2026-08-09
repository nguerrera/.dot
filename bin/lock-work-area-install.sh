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

# The uuids of the list key last read. The edits below work in these rather
# than in the printed form.
ITEMS=()

# gsettings prints a list of strings as "['a@b', 'c@d']", and an empty one as
# "@as []", carrying the type it cannot infer from no elements. A uuid holds
# neither a comma nor a quote, so splitting on the comma recovers the elements.
read_list() {
    local value parts part

    value="$(gsettings get "$SCHEMA" "$1")" || return $?
    value="${value#@as }"
    value="${value#\[}"
    value="${value%\]}"

    IFS=, read -ra parts <<< "$value"
    ITEMS=()

    for part in "${parts[@]}"; do
        part="${part# }"
        part="${part#\'}"
        part="${part%\'}"

        if [[ -n "$part" ]]; then
            ITEMS+=("$part")
        fi
    done
}

write_list() {
    local value="" item

    for item in "${ITEMS[@]}"; do
        if [[ -n "$value" ]]; then
            value+=", "
        fi

        value+="'$item'"
    done

    gsettings set "$SCHEMA" "$1" "[$value]"
}

has_uuid() {
    local item

    for item in "${ITEMS[@]}"; do
        if [[ "$item" == "$UUID" ]]; then
            return 0
        fi
    done

    return 1
}

list_add() {
    read_list "$1" || return $?

    if has_uuid; then
        echo "$UUID is already in $SCHEMA $1."
        return 0
    fi

    ITEMS+=("$UUID")
    write_list "$1" || return $?
    echo "Added $UUID to $SCHEMA $1."
}

list_remove() {
    local kept=() item

    read_list "$1" || return $?

    if ! has_uuid; then
        echo "$UUID is not in $SCHEMA $1."
        return 0
    fi

    for item in "${ITEMS[@]}"; do
        if [[ "$item" != "$UUID" ]]; then
            kept+=("$item")
        fi
    done

    ITEMS=("${kept[@]}")
    write_list "$1" || return $?
    echo "Removed $UUID from $SCHEMA $1."
}

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

# The shell runs an extension only where both of its lists agree that it is on:
# the uuid in enabled-extensions, and out of disabled-extensions, which is
# where gnome-extensions disable leaves it. Editing the lists rather than
# asking the shell also sidesteps a race on a first install. The shell notices
# a new extension directory through a file monitor, and gnome-extensions enable
# fails on a uuid it has not seen yet; the lists are read at startup, which
# does not depend on winning that race.
list_add enabled-extensions || exit $?
list_remove disabled-extensions || exit $?

echo "Log out and back in for this to take effect. The shell caches an"
echo "extension's JavaScript once it has imported it, and disabling the"
echo "extension does not reload the file."
