#!/usr/bin/env bash

xrdb -merge ~/.Xresources

if have prlcc; then
    prlcc &
fi

exec /bin/i3
