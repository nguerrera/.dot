Import-Module posh-git
Import-Module oh-my-posh
Set-Theme Paradox

$global:DefaultUser='nicholg'

# Disable slow file status on prompt
$GitPromptSettings.EnableFileStatus = $false

# Disable icon that misleads when file status is disabled
$ThemeSettings.GitSymbols.BranchUntrackedSymbol = ''

# Don't change the window title when I change directories
$ThemeSettings.Options.ConsoleTitle = $false

Set-PSReadLineOption -EditMode Emacs -BellStyle Visual
Set-PSReadlineKeyHandler -Key Tab -Function TabCompleteNext
Set-PSReadlineKeyHandler -Key Shift+Tab -Function TabCompletePrevious

function Test-Command {
    param($command)
    Get-Command -ErrorAction SilentlyContinue $Command | Out-Null
    $?
}

# Helper to define aliases in a more bash-like way
#
# Usage: `_alias somecommand someothercommand a b c`
#
# This will turn `somecommand x y z` into `someothercommand a b c x y z` when
# run interactively at the top level.
#
# However, we go to great pain to avoid changing behavior of arbitrary functions
# or scripts that use them. As such, we can alias common commands like ls, and
# (barring bugs below and extreme edge cases), scripts will continue to behave
# as before.
#
# Incidentally, this is call _alias, not alias because `alias` is implicitly
# Get-Alias by default and I didn't want to break that either.
#
# NOTE: This must be called from top-level or the resulting alias won't be visible.
#       Use -if to condition.
function _alias {
    param(
        $if = $true,

        [Parameter(Mandatory=$true, Position=0)]
        $command, 
        
        [Parameter(Mandatory=$true, Position=1, ValueFromRemainingArguments=$true)]
        [string[]]
        $args
    )

    if (!$if) {
        return;
    }

    if ((Test-Path "Alias:$command") -and !(Test-Path "Function:\global:$command")) {
        # When there's already an alias, convert it to a wrapper function. This
        # is what scripts will see after we replace the alias with a private
        # one.
        $currentAlias = Get-Alias $command
        $currentTarget = $currentAlias.ResolvedCommand
        $block = [scriptblock]::create("process { `$_ | & $currentTarget @args }");
        New-Item "Function:\global:$command" -Value $block | Out-Null
    }

    # Remove any existing aliases. Some aliases are hard to kill. If they have
    # AllScope, it takes more than one go at it, and ReadOnly requires -Force.
    while (Test-Path "Alias:$command") {
        Remove-Alias -Force $command
    }

    if ($args.Length -eq 1) {
        Set-Alias -Scope Global -Option Private -Name $command -Value $args[0]
        return;
    }

    # PowerShell aliases can't have args, so generate a function to pass the
    # args and alias that
    if (Test-Path "Function:\global:${command}-shim") {
        Remove-Item "Function:\global:${command}-shim"
    }
    $block = [scriptblock]::Create("& $args @args")
    New-Item "Function:\global:${command}-alias" -Value $block | Out-Null
    Set-Alias -Scope Global -Option Private -Name $command -Value "${command}-alias" | Out-Null
}

# Load VS developer environment
#
# NOTE: This is slow and I don't need it so much these days, so tuck it behind a
# helper function that can be invoked when needed.
function vsenv {
    $vswhere = 'C:\Program Files (x86)\Microsoft Visual Studio\Installer\vswhere.exe'
    if ($args.Length -eq 0) {
        $args = @('-latest', '-prerelease')
    }

    if (Test-Path $vswhere) {
        $installPath = & $vswhere @args -property installationPath
        if ($installPath) { 
          Import-Module (Join-Path $installPath 'Common7\Tools\Microsoft.VisualStudio.DevShell.dll')
          Enter-VsDevShell -VsInstallPath $installPath -SkipAutomaticLocation
        }
    }
}

# add Beyond Compare to path if found
if (Test-Path "${Env:ProgramW6432}\Beyond Compare 4") {
  $Env:PATH="${Env:PATH};%ProgramW6432%\Beyond Compare 4"
}

# add this directory to PATH
$THIS_DIR = Split-Path -Parent -Path $MyInvocation.MyCommand.Definition
$Env:PATH="$THIS_DIR;${Env:PATH}"

# add Git directory to PATH
$GIT_DIR=$null
if (Test-Path "${Env:ProgramW6432}\Git") {
    $GIT_DIR="${Env:ProgramW6432}\Git"
    # at the end to avoid conflicts such as find.exe breaking Windows things
    $Env:PATH="${Env:PATH};$GIT_DIR\mingw64\bin;$GIT_DIR\usr\bin"
}
 
# use hub as alias for git if available
$HUB_DIR=$null
if (Test-Path "${Env:UserProfile}\OneDrive\Tools\Hub\bin") {
    $HUB_DIR="${Env:UserProfile}\OneDrive\Tools\Hub\bin"
}
_alias -if $HUB_DIR git "$HUB_DIR\hub.exe"


# Same as Where-Object except when called with no pipeline input and no switches,
# then it behaves like bash type or windows where.exe.
function _where {
    process {
        if ($_ || $args.Length -ne 1 || $args[0].StartsWith('-')) {
            $_ | Where-Object @args
        } else {
            Get-Command -All @args
        }
    }
}

# This overrides the tgit from posh-git, which doesn't do /path for you
function tgit {
    param (
        [Parameter(Mandatory=$true, Position=0)]
        $command,

        [Parameter(Position=1)]
        $path,

        [Parameter(ValueFromRemainingArguments=$true)]
        $args
    )
    if ($path) {
        $path="/path:$path"
    }
    & $Global:TortoiseGitSettings.TortoiseGitPath /command:$command $path @args
}

function .. { Set-Location .. }

# TODO: These don't find my private aliases due to scoping
_alias where _where
_alias type Get-Command -All
_alias which Get-Command -All

# Don't hide unix ls from me
 _alias -if (Test-Command ls.exe) `
    ls ls.exe --color -h -F --ignore="ntuser.*" --ignore="NTUSER.*" --ignore="*fil*.sys"

# Nor cmd dir, which can be much faster when recursive
_alias dir cmd /c dir

# Nor cmd rd, which is also much faster when recursive
_alias rd cmd /c rd

# wrist friendlier gci since we took back dir and ls above
_alias ll Get-ChildItem 

# Prefer code-insiders over code
_alias -if (Test-Command code-insiders) code code-insiders

# Prefer unix find over windows find
_alias -if $GIT_DIR find "$GIT_DIR\usr\bin\find.exe"
_alias wfind "${Env:SystemRoot}\system32\find.exe"
_alias h history
_alias du du -h
_alias df df -h
_alias emacs emacsclient.cmd -n @args
_alias e emacs-alias
_alias vi emacs-alias
_alias n emacs-alias
_alias notepad emacs-alias
_alias ms emacsclient.cmd `-e '"(progn (magit-status) (raise-frame))"'



