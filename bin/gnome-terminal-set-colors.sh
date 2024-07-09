#!/usr/bin/env bash

# My custom colors for gnome-terminal

# Based on:
#   https://github.com/shaneholloman/onedark-gnome-terminal/
# Which was based on:
#   https://github.com/chriskempson/base16-gnome-terminal/

black="#282C34"
blue="#61AFEF"
brightBlack="#8D8D8D"
brightBlue="#8AC1EE"
brightCyan="#8EB7BD"
brightGreen="#A9CE8F"
brightPurple="#DA9DEC"
brightRed="#E0606B"
brightWhite="#DCDFE4"
brightYellow="#F3DAAB"
cyan="#56B6C2"
green="#98C379"
purple="#C678DD"
red="#E06C75"
white="#ABB2BF"
yellow="#E5C07B"

[[ -z "$PROFILE_NAME" ]] && PROFILE_NAME="Custom"
[[ -z "$PROFILE_SLUG" ]] && PROFILE_SLUG="custom"
[[ -z "$DCONF" ]] && DCONF=dconf
[[ -z "$UUIDGEN" ]] && UUIDGEN=uuidgen

dset() {
    local key="$1"; shift
    local val="$1"; shift

    if [[ "$type" == "string" ]]; then
        val="'$val'"
    fi

    "$DCONF" write "$PROFILE_KEY/$key" "$val"
}

# because dconf still doesn't have "append"
dlist_append() {
    local key="$1"; shift
    local val="$1"; shift

    local entries="$(
        {
            "$DCONF" read "$key" | tr -d '[]' | tr , "\n" | fgrep -v "$val"
            echo "'$val'"
        } | head -c-1 | tr "\n" ,
    )"

    "$DCONF" write "$key" "[$entries]"
}

if which "$DCONF" > /dev/null 2>&1; then
    [[ -z "$BASE_KEY_NEW" ]] && BASE_KEY_NEW=/org/gnome/terminal/legacy/profiles:

    if [[ -n "`$DCONF list $BASE_KEY_NEW/`" ]]; then
        if which "$UUIDGEN" > /dev/null 2>&1; then
            PROFILE_SLUG=`uuidgen`
        fi

        if [[ -n "`$DCONF read $BASE_KEY_NEW/default`" ]]; then
            DEFAULT_SLUG=`$DCONF read $BASE_KEY_NEW/default | tr -d \'`
        else
            DEFAULT_SLUG=`$DCONF list $BASE_KEY_NEW/ | grep '^:' | head -n1 | tr -d :/`
        fi

        DEFAULT_KEY="$BASE_KEY_NEW/:$DEFAULT_SLUG"
        PROFILE_KEY="$BASE_KEY_NEW/:$PROFILE_SLUG"

        # copy existing settings from default profile
        $DCONF dump "$DEFAULT_KEY/" | $DCONF load "$PROFILE_KEY/"

        # add new copy to list of profiles
        dlist_append $BASE_KEY_NEW/list "$PROFILE_SLUG"

        # update profile values with theme options
        dset visible-name "'$PROFILE_NAME'"
        dset palette "['$black', '$red', '$green', '$yellow', '$blue', '$purple', '$cyan', '$white', '$brightBlack', '$brightRed', '$brightGreen', '$brightYellow', '$brightBlue', '$brightPurple', '$brightCyan', '$brightWhite']"
        dset background-color "'$black'"
        dset foreground-color "'$white'"
        dset bold-color "'$white'"
        dset bold-color-same-as-fg "true"
        dset use-theme-colors "false"
        dset use-theme-background "false"

        exit 0
    fi
fi

exit 1
