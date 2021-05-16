#!/usr/bin/env bash

# Wrapper around emacsclient that works as an alternative to using emacs
# --daemon or ALTERNATE_EDITOR, which both caused various issues for me.
# Instead, we lazily create a minimized emacs instance to act as the server
# if a client command comes in and emacs is not running yet.

declare -a startArgs
declare -a clientArgs
declare -a clientArgsNoFrame

createFrame=false
fileArg=false
takeBackFocus=false
startMinimized=true
emacs=emacs
emacsclient=emacsclient

if [ -d /Applications/Emacs.app ]; then
  emacs=/Applications/Emacs.app/Contents/MacOS/Emacs
  emacsclient=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient

  # Mac OS has a weird behavior where it restores another minimized frame
  # when closing the client frame instead of closing it and returning focus.
  # To deal with this, we reuse the initial frame for first invocation and
  # use AppleScript to get the focus back.
  currentApp=`osascript -e "(path to frontmost application) as text"`
  takeBackFocus=true
  startMinimized=false
fi

for arg; do
    case $arg in
        -c|--create-frame)
            createFrame=true
            clientArgs+=("$arg")
            ;;
        -n|--no-wait)
            takeBackFocus=false
            clientArgs+=("$arg")
            clientArgsNoFrame+=("$arg")
            ;;
        -*)
            clientArgs+=("$arg")
            clientArgsNoFrame+=("$arg")
            ;;
        *)
            fileArg=true
            clientArgs+=("$arg")
            clientArgsNoFrame+=("$arg")
            ;;
    esac
    shift
done

# if not given a file and not told to create a frame, bring emacs into focus
if ((! $fileArg) && (! $createFrame)); then
    clientArgs=(--eval "(raise-frame)")
fi

# if we're creating a frame, then start the initial instance minimized
if $createFrame && $startMinimized; then
    startArgs=(--iconic)
fi

# check if emacs server is up
ping() {
    $emacsclient --eval nil > /dev/null 2>&1
}

if ! ping; then
    # start a new emacs
    $emacs "${startArgs[@]}" &

    # if we're not starting the new instance minimized then don't create
    # another frame for the client
    if ! $startMinimized; then
        clientArgs=("${clientArgsNoFrame[@]}")
    fi
    [ $? -eq 0 ] || exit $?

    # wait for emacs server to respond
    echo -n "Waiting for Emacs server to start.."
    while true; do
        if ping; then
            break
        fi
        echo -n "."
        sleep 1
    done
    echo
fi

if $takeBackFocus; then
    $emacsclient "${clientArgs[@]}"
    exitCode=$?
    osascript -e "activate application \"$currentApp\""
    exit $exitCode
fi

exec $emacsclient "${clientArgs[@]}"