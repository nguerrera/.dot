# do nothing if not running interactively
[ -z "$PS1" ] && return

# source global /etc/bashrc and /etc/bash_completion if available
for etc in /etc /usr/local/etc; do
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

# Set up the prompt with some defense against dumb terminals
# that don't understand the color escape sequences (e.g. M-x
# shell on the old version of emacs that ships with Mac OS X.)
__set-prompt () {
    local colors=$(tput colors 2> /dev/null)
    if [ ${colors} -ge 8 ] 2> /dev/null || [ "$TERM" == "cygwin" ]; then
        local cyan='\e[36m'
        local yellow='\e[33m'
        local plain='\e[0m'
    fi

    local ps1_dir='\n\u@\h:\w'

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

# add custom git commands to path
export PATH=$PATH:$HOME/.dot/git

# editor
if [ -d /Applications/Emacs.app ]; then
    export ALTERNATE_EDITOR=/Applications/Emacs.app/Contents/MacOS/Emacs
    export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
    alias emacs='$EDITOR -n'
    alias e=emacs
elif have emacs; then
    export EDITOR=emacs
    alias e=emacs
elif have code-insiders; then
    export EDITOR='code-insiders -w'
    alias emacs=code-insiders
    alias e=emacs
elif have code; then
    export EDITOR='code -w'
    alias emacs=code
    alias e=emacs
else
    alias emacs=vi
    alias e=vi
fi

# prefer vs code insiders
if have code-insiders; then
    alias code=code-insiders
fi

if have code; then
    alias n=code
else
    alias n=emacs
fi

alias notepad=n

# install global npm packages to user dir
if have npm; then
  if [ ! -d $HOME/.npm/g ]; then
    mkdir -p $HOME/.npm/g && npm config set prefix $HOME/.npm/g
  fi
  export PATH=$HOME/.npm/g/bin:$PATH
fi

# add dotnet global tools to path
if have dotnet; then
    export PATH=$HOME/.dotnet/tools:$PATH
fi

# aliases
alias -- -='cd -'
alias ..='cd ..'
alias cls=clear
alias cp='cp -i'
alias df='df -h'
alias dir='ls -l'
alias du='du -h'
alias h=history
alias ll='ls -l'
alias ln='ln -i'
alias ls='ls $LS_OPTIONS'
alias md=mkdir
alias mv='mv -i'
alias rd=rmdir
alias rm='rm -i'
alias ren=mv
alias where='type -a'
alias which='where'
alias tracert=traceroute
alias copy=cp
alias move=mv
alias del=rm

# WSL interop
if have cmd.exe; then
    tgit() {
        local patharg
        if [ $# -lt 2 ]; then
           patharg=$(git rev-parse --show-toplevel)
        else
           patharg=$2
        fi
        patharg=$(wslpath -a -w $patharg)
        "/mnt/c/Program Files/TortoiseGit/bin/TortoiseGitProc.exe" /command:$1 /path:$patharg
    }

    start() {
        local arg=$1
        if [ -e $arg ]; then
           arg=$(wslpath -a -w $arg)
        fi
        # cmd does not like being run from a network path
        pushd /mnt/c > /dev/null
        cmd.exe /c start $arg
        # annoyingly, above changes terminal title
        cmd.exe /c title Terminal
        popd > /dev/null
    }

    alias open=start
    alias bcomp=~/.dot/git/bcomp-wsl
    alias ms='emacs -nw --eval "(magit-status)"'

    # Make ls gnore paths that cause permission denied in WSL
    WSL_LS_OPTIONS="--ignore=ntuser.* --ignore=NTUSER.* --ignore=*fil*.sys --ignore=DumpStack.log.tmp --ignore=Config.Msi --ignore=Recovery --ignore=System*Volume*Information'"

    # Prevent ls from highlighting everything in /mnt/c
    export LS_COLORS=$LS_COLORS:'ow=1;34:'
fi

if have hub; then
    alias git=hub
fi

if ! have start; then
    if have xdg-open; then
        alias start=xdg-open
        alias open=start
    elif have open; then
        alias start=open
    fi
fi

# Start with BSD-safe LS_OPTIONS.
# We'll augment them if we find GNU coreutils below.
# These LSCOLORS are designed to match the GNU defaults.
export CLICOLOR=1
export LSCOLORS=exgxbxdxcxegedabagacad
LS_OPTIONS="-h -F $WSL_LS_OPTIONS"


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
            local coreutils=/usr/local/opt/coreutils/libexec
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
    LS_OPTIONS="$LS_OPTIONS --color=auto"
    if ls ~/ --group-directories-first > /dev/null 2>&1; then
        LS_OPTIONS="$LS_OPTIONS --group-directories-first"
    fi
fi

unset -f __use-gnu-coreutils
