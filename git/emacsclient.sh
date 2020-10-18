#!/usr/bin/env bash

declare -a startArgs
declare -a clientArgs
createFrame=false
fileArg=false

for arg; do
    case $arg in
        -c|--create-frame)
            createFrame=1
            ;;
        -*)
            ;;
        *)
            fileArg=true
            ;;
    esac
    clientArgs+=("$arg")
    shift
done

# if not given a file and not told to create a frame, bring emacs into focus
if ((! $fileArg) && (! $createFrame)); then
    clientArgs=(--eval "(raise-frame)")
fi

# if we're creating a frame, then start the initial instance minimized
if $createFrame; then
    startArgs=(--iconic)
fi

# check if emacs server is up
ping() {
  emacsclient --eval nil > /dev/null 2>&1
}

if ! ping; then
    # start a new emacs
    emacs "${startArgs[@]}" &
    [ $? -eq 0 ] || exit $?

    # wait for emacs server to respond
    echo -n "Waiting for Emacs server to start..."
    while true; do
        if ping; then
            break
        fi
        echo -n "."
        sleep 1
    done
    echo
fi

exec emacsclient "${clientArgs[@]}"
