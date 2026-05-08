# Determine if the given command is available
have() {
    command -v $1 > /dev/null 2>&1
}

# add custom bin dir and local bin to path
export PATH=$HOME/.dot/bin:$HOME/.local/bin:$PATH

# add dnvm env
if ! have dotnet && [ -d "$HOME/.local/share/dnvm" ]; then
    export PATH="$HOME/.local/share/dnvm:$PATH"
    export PATH="$HOME/.dotnet/tools:$PATH"
    export DOTNET_ROOT="$HOME/.local/share/dnvm/dn"
fi

# add homebrew env
if [ -x /opt/homebrew/bin/brew ]; then
    eval $(/opt/homebrew/bin/brew shellenv)
fi

# Add beyond compare to PATH
if [ -d /Applications/Beyond\ Compare.app/Contents/MacOS ]; then
   export PATH=$PATH:/Applications/Beyond\ Compare.app/Contents/MacOS
fi

# Use node@24 on homebrew if we don't have node on PATH, but we have that
if ! have node && [ -d /opt/homebrew/opt/node@24/bin ]; then
    export PATH=$PATH:/opt/homebrew/opt/node@24/bin
fi

# install global npm packages to user dir so I can `npm install -g` without
# sudo, mirrors default behavior on Windows
if have npm; then
    # don't change anything if git bash or wsl are using the win32 npm
    case "$(command -v npm)" in
        *Program\ Files*) ;;
        *)
            if [ ! -d $HOME/.npm/g ]; then
                mkdir -p $HOME/.npm/g && npm config set prefix $HOME/.npm/g
            fi
            export PATH=$HOME/.npm/g/bin:$PATH
            ;;
    esac
fi

# add dotnet global tools to path
if have dotnet; then
    export PATH=$HOME/.dotnet/tools:$PATH
fi

# source .bashrc if running bash
if [ -n "$BASH_VERSION" ] && [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

