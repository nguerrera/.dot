# do nothing if not running interactively
[ -z "$PS1" ] && return

# source global /etc/bashrc and /etc/bash_completion if available
for etc in /etc /usr/local/etc /opt/homebrew/etc; do
    for config in bashrc bash_completion; do
        if [ -f $etc/$config ]; then
           source $etc/$config
           continue
        fi

        if [ -d $etc/$config.d ]; then
            for f in $etc/$config.d/*; do
                source $f;
            done
        fi
    done
done

# Determine if the given command is available
have() {
    type -t $1 > /dev/null 2>&1
}

# Set the title for a terminal window
title() {
  echo -ne "\033]0;$1\007"
}

# Set up the prompt with some defense against dumb terminals
# that don't understand the color escape sequences (e.g. M-x
# shell on the old version of emacs that ships with Mac OS X.)
__set-prompt () {
    local colors=$(tput colors 2> /dev/null)
    if [ ${colors} -ge 8 ] 2> /dev/null || [ "$TERM" == "cygwin" ]; then
        local cyan='\e[36m'
        local green='\e[32m'
        local yellow='\e[33m'
        local plain='\e[0m'
    fi

    if have cmd.exe; then
        case $(uname) in
            MINGW*|MSYS*)
                local ps1_dir="\n${cyan}MSYS:\w"
                ;;
            Linux)
                local ps1_dir="\n${cyan}WSL:\w"
                ;;
        esac
    else
        local ps1_dir="\n\u@\h:\w"
    fi

    if [ "$(type -t __git_ps1)" != "function" ]; then
        if [ -f /usr/share/git/git-prompt.sh ]; then
            source /usr/share/git/git-prompt.sh
        fi
    fi

    if [ "$(type -t __git_ps1)" == "function" ]; then
        # using $(...) instead of `...` here does not work in MSYS2
        local ps1_git='`__git_ps1 " (%s)"`'
    fi

    local ps1_prompt='\n\$ '
    PS1="${cyan}${ps1_dir}${yellow}${ps1_git}${plain}${ps1_prompt}"
}
__set-prompt
unset -f __set-prompt

# don't include repeat commands in history 
HISTCONTROL=ignoredups:erasedups

# cd to directory when given as bare command
shopt -s autocd 2> /dev/null

# expand variables in directory completion, don't escape as literal $
shopt -s direxpand 2> /dev/null

# expand recursive **/ glob patterns
shopt -s globstar 2> /dev/null

# ignore case when globbing
shopt -s nocaseglob 2> /dev/null

# add custom bin dir to path
export PATH=$HOME/.dot/bin:$PATH

# add homebrew env
if [ -x /opt/homebrew/bin/brew ]; then
    eval $(/opt/homebrew/bin/brew shellenv)
fi

# editor
if [ -d /Applications/Emacs.app ]; then
   export EDITOR="$HOME/.dot/bin/emacsclient.sh -c"
   alias emacsclient="$HOME/.dot/bin/emacsclient.sh"
   alias emacs='/Applications/Emacs.app/Contents/MacOS/emacs'
   alias e='emacsclient -n'
   alias ms='emacsclient --eval "(progn (magit-status) (raise-frame))"'
elif have emacs; then
    if [ "$DISPLAY" != "" ] && (! have wslpath); then
      export EDITOR="$HOME/.dot/bin/emacsclient.sh -c"
      alias emacsclient="$HOME/.dot/bin/emacsclient.sh"
      alias e='emacsclient -n'
      alias ms='emacsclient --eval "(progn (magit-status) (raise-frame))"'
    else
      export EDITOR=emacs
      alias e=emacs
      alias ms='emacs --eval "(magit-status)"'
    fi
elif have code-insiders; then
    export EDITOR='code-insiders -w'
    alias e=code-insiders
elif have code; then
    export EDITOR='code -w'
    alias e=code
else
    alias e=vi
fi

# prefer vs code insiders
if have code-insiders; then
    alias code=code-insiders
fi

if have code; then
    alias n=code
else
    alias n=e
fi

if ! have python && have python3; then
    alias python=python3
fi

export SUDO_EDITOR=$EDITOR
alias se=sudoedit
alias notepad=n

# Add beyond compare to PATH
if [ -d /Applications/Beyond\ Compare.app/Contents/MacOS ]; then
   export PATH=$PATH:/Applications/Beyond\ Compare.app/Contents/MacOS
fi

# Use node@22 on homebrew if we don't have node on PATH, but we have that
if ! have node && [ -d /opt/homebrew/opt/node@22/bin ]; then
    export PATH=$PATH:/opt/homebrew/opt/node@22/bin
fi

# install global npm packages to user dir so I can `npm install -g` without
# sudo, mirrors default behavior on Windows
if have npm; then
    # don't change anything if git bash or wsl are using the win32 npm
    if [[ "$(type -f -p npm)" != *"Program Files"* ]]; then
        if [ ! -d $HOME/.npm/g ]; then
            mkdir -p $HOME/.npm/g && npm config set prefix $HOME/.npm/g
        fi
        export PATH=$HOME/.npm/g/bin:$PATH
    fi
fi

# add dotnet global tools to path
if have dotnet; then
    export PATH=$HOME/.dotnet/tools:$PATH
fi

# aliases
alias -- -='cd -'
alias ..='cd ..'
alias cls=clear
alias df='df -h'
alias dir='ls -l'
alias du='du -h'
alias h=history
alias ll='ls -l'
alias ls='ls $LS_OPTIONS'
alias md=mkdir
alias rd=rmdir
alias ren=mv
alias where='type -a'
alias which='where'
alias tracert=traceroute
alias copy=cp
alias move=mv
alias del=rm

# WSL / MSYS interop
if have cmd.exe; then
    tgit() {
        local patharg
        if [ $# -lt 2 ]; then
           patharg=$(git rev-parse --show-toplevel)
        else
           patharg=$2
        fi
        if have wslpath; then
            patharg=$(wslpath -a -w $patharg)
            "/mnt/c/Program Files/TortoiseGit/bin/TortoiseGitProc.exe" /command:$1 /path:$patharg
        else
            patharg=$(cygpath -a -w $patharg)
            "/c/Program Files/TortoiseGit/bin/TortoiseGitProc.exe" /command:$1 /path:$patharg
        fi
    }

    if have wslview; then
        start() {
           wslview $*
           local error_code=$?
           # Annoyingly wslview -> powershell changes the terminal title, so change it back
           title Terminal
           return $error_code
        }
    fi

    alias bcomp=bcomp-wsl

    # Hide well-known windows hidden files
    WIN_LS_OPTIONS=" \
        --ignore=*fil*.sys \
        --ignore=?Recycle.Bin \
        --ignore=?RECYCLE.BIN \
        --ignore=?SysReset \
        --ignore=?WinREAgent \
        --ignore=Application?Data \
        --ignore=bootmgr \
        --ignore=BOOTNXT \
        --ignore=Config.Msi \
        --ignore=Documents?and?Settings \
        --ignore=DumpStack.log.tmp \
        --ignore=Local?Settings \
        --ignore=My?Documents \
        --ignore=ntuser.* \
        --ignore=NTUSER.* \
        --ignore=OneDriveTemp \
        --ignore=Recovery \
        --ignore=system.sav \
        --ignore=System?Volume?Information \
        --ignore=?WINRE_BACKUP_PARTITION.MARKER \
        --ignore=desktop.ini \
        "

    # Prevent ls from highlighting everything in /mnt/c
    export LS_COLORS=$LS_COLORS:'ow=1;34:'
fi

if ! have start; then
    if have xdg-open; then
        alias start=xdg-open
        alias open=start
    elif have open; then
        alias start=open
    fi
elif ! have open; then
    alias open=start
fi

# Add completion for the dotnet
_dotnet_bash_complete() {
  local cur="${COMP_WORDS[COMP_CWORD]}" IFS=$'\n'
  local candidates
  read -d '' -ra candidates < <(dotnet complete --position "${COMP_POINT}" "${COMP_LINE}" 2>/dev/null)
  read -d '' -ra COMPREPLY < <(compgen -W "${candidates[*]:-}" -- "$cur")
}
complete -f -F _dotnet_bash_complete dotnet

# Start with BSD-safe LS_OPTIONS.
# We'll augment them if we find GNU coreutils below.
# These LSCOLORS are designed to match the GNU defaults.
export CLICOLOR=1
export LSCOLORS=exgxbxdxcxegedabagacad
LS_OPTIONS="-h -F $WIN_LS_OPTIONS"

# Use GNU coreutils where possible
#
# I greatly prefer GNU coreutils over the spartan BSD equivalents.
#
# On Mac OS X, install them via coreutils, which puts a g prefix in
# front of evertything by default.
#
# One way around that is to put the gnubin/ folder on the front of
# PATH, but then it's easy to take a dependency on GNU extensions in a
# script that was meant to run everywhere. Instead, the code below
# generates aliases for every coreutils command.
#
# With this setup, you can use still use a leading \ to get around the
# aliases to use the system utils.
#
# e.g.:
#
#    ls      -> runs gnu ls
#    \ls     -> runs BSD ls
#    man ls  -> shows GNU ls man page
#    \man ls -> shows BSD ls man page
#
# This escape hatch is handy during the development of scripts that
# need to be portable to BSD system without GNU coreutils.
#
__use-gnu-coreutils() {
    case $(uname) in
        GNU*|Linux|MINGW*|MSYS*)
            # GNU coreutils provided by the system
            return 0
            ;;
        
        Darwin)
            local coreutils=/opt/homebrew/opt/coreutils/libexec
            if [ ! -d $coreutils ]; then
                # coreutils have not been installed
                return 1
            fi

            for f in $coreutils/gnubin/*; do
                local cmd=${f##**/}
                
                if [ "$(type -t $cmd)" == "builtin" ]; then
                    # Don't try to alias built-in's like [ to g[
                    continue
                fi
	        
                local a=$(alias $cmd 2> /dev/null)
                if [ "$a" ]; then
                    # There's an existing alias, such as cp='cp -i'
                    # Munge it to cp='gcp -i'   
                    eval ${a/\'$cmd/\'g$cmd}
                else
                    alias $cmd=g$cmd
                fi
            done

            # gnu-tar is a separate package and not handled by above
            if have gtar; then
              alias tar=gtar
            fi
            
            eval "alias man='MANPATH=$coreutils/gnuman:$MANPATH man'"
            return 0
            ;;
        
        *)
            # non-GNU, non-Mac OS system -- don't bother
            return 1
            ;;
    esac
}

if __use-gnu-coreutils; then
    LS_OPTIONS="$LS_OPTIONS --color=auto --group-directories-first"
fi

unset -f __use-gnu-coreutils
