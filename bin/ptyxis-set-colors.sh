#!/usr/bin/env bash

# My custom colors for Ptyxis (GNOME 50+)
#
# Imports etc/ptyxis.palette into Ptyxis and applies it to the default profile.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PALETTE_FILE="$SCRIPT_DIR/../etc/ptyxis-ng.palette"
# Ptyxis palette ID is the filename stem, not the Name= field in the file
PALETTE_ID="$(basename "$PALETTE_FILE" .palette)"

[[ -z "$DCONF" ]] && DCONF=dconf

if ! which ptyxis > /dev/null 2>&1; then
    echo "Error: ptyxis not found" >&2
    exit 1
fi

if ! which "$DCONF" > /dev/null 2>&1; then
    echo "Error: dconf not found" >&2
    exit 1
fi

if [[ ! -f "$PALETTE_FILE" ]]; then
    echo "Error: palette file not found: $PALETTE_FILE" >&2
    exit 1
fi

# --import-palette errors if the file already exists; remove it first
DEST="$HOME/.local/share/org.gnome.Ptyxis/palettes/$(basename "$PALETTE_FILE")"
rm -f "$DEST"

# Import the palette into Ptyxis
ptyxis --import-palette "$PALETTE_FILE"

# Apply the palette to the default profile
BASE_KEY=/org/gnome/Ptyxis
DEFAULT_UUID="$("$DCONF" read "$BASE_KEY/default-profile-uuid" | tr -d "'")"

if [[ -z "$DEFAULT_UUID" ]]; then
    echo "Error: no default profile found" >&2
    exit 1
fi

"$DCONF" write "$BASE_KEY/Profiles/$DEFAULT_UUID/palette" "'$PALETTE_ID'"
echo "Palette '$PALETTE_ID' applied to profile $DEFAULT_UUID."
