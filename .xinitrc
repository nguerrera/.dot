#!/usr/bin/env bash

xrdb -merge ~/.Xresources

if type -t prlcc > /dev/null 2>&1; then
    prlcc &
fi

exec /bin/i3
