using namespace System.Management.Automation
using namespace System.Management.Automation.Language
using namespace System.Collections.Generic
using namespace System.Security.Principal
using namespace Microsoft.PowerShell

. $PSScriptRoot\tiny-posh-git.ps1

# Clean up after prior invocations
if (!$Env:_NG_PATH_BEFORE_PROFILE) {
    $Env:_NG_PATH_BEFORE_PROFILE = $Env:PATH
}
$Env:PATH = $Env:_NG_PATH_BEFORE_PROFILE

# Check if a given command is available
function Test-Command([string] $command) {
    Get-Command -ErrorAction SilentlyContinue $Command | Out-Null
    return $?
}

# Check if we're running as admin
function Test-Admin {
    $principal = [WindowsPrincipal][WindowsIdentity]::GetCurrent()
    return $principal.IsInRole([WindowsBuiltInRole]'Administrator')
}

# Check if given string looks like a unix arg (--something or -x)
function Test-UnixArg([string] $arg) {
    return $arg -and ($arg.StartsWith('--') -or (($arg.Length -eq 2) -and $arg[0] -eq '-'))
}

# Check if the given string looks like a cmd arg
function Test-CmdArg([string] $arg) {
    if (!$arg -or $arg.Length -lt 2 -or $arg[0] -ne '/') {
        return $false
    }

    # special case for dir /s/b without space between switches
    if ($arg -eq "/s/b") {
        return $true
    }

    # /x
    if ($arg.Length -eq 2) {
        return $true
    }

    # /x:y
    if ($arg.Length -eq 4) {
        return $arg[2] -eq ':'
    }

    return $false
}

# Register a macro to replace the given command when it appears as the first
# token of command line is $command, replace it with $replacement.
#
# $replacement can be a string or a scriptblock taking the command line
# split into tokens that returns the replacement for the or opts out of
# replacing by returning $null.
#
# These macros are invisible to scripts or in any position beyond the start
# of the command line. This allows us to customize built-in commands without
# breaking anything.
$Macros = @{}
function Set-Macro([string] $command, $replacement) {
    $Macros[$command] = $replacement
}

# Replace command line according to registered macro
function Expand-Macros {
    $tokens = Get-PSReadLineTokens

    # first token is command, last token is EOF
    if ($tokens.Count -lt 2) {
        return
    }

    $extent = $tokens[0].Extent

    # If the first token is 'time', skip it so that time can time macros
    if ($extent.Text -eq 'time') {
        if ($tokens.Count -lt 3) {
            return
        }
        $extent = $tokens[1].Extent
        $tokens = $tokens[1 .. ($tokens.Length)]
    }

    $command = $extent.Text
    $replacement = $Macros[$command]
    if (!$replacement) {
        return
    }

    if ($tokens.Length -eq 2) {
        $arguments = @()
    } else {
        $arguments = [string[]]($tokens[1 .. ($tokens.Length - 2)] | ForEach-Object { $_.Extent.Text })
    }
    if ($replacement -is [scriptblock]) {
        $replacement = & $replacement $command @arguments
    }

    if ($replacement) {
        [PSConsoleReadLine]::Replace($extent.StartOffset, $extent.EndOffset - $extent.StartOffset, $replacement)
        [PSConsoleReadLine]::EndOfLine()
    }
}

function Get-PSReadLineTokens {
    $ast = $null
    $tokens = $null
    $errors = $null
    $cursor = $null
    [PSConsoleReadLine]::GetBufferState([ref]$ast, [ref]$tokens, [ref]$errors, [ref]$cursor)
    return $tokens
}

function Get-PSReadLineBuffer {
    $buffer = $null
    $cursor = $null
    [PSConsoleReadLine]::GetBufferState([ref]$buffer, [ref]$cursor)
    return $buffer
}

# Record command line before and after history search
$HistoryState = @{ 
    CmdBefore = ""
    CmdAfter = ""
}

# Make Clear-History actually clear everything
function Clear-History {
    [PSConsoleReadLine]::ClearHistory()
    Remove-Item (Get-PSReadLineOption).HistorySavePath
    & (Get-Command -CommandType Cmdlet Clear-History)
}


# Wrapper around Set-PSReadLineKeyHandler to add macro expansion hook
function Set-PSReadLineKeyHandlerWithMacroExpansion($key, $func) {
    $description = if ($func -ne '') { "ExpandMacrosAnd${func}" } else { "ExpandMacros" }
    Set-PSReadLineKeyHandler -Key $key -BriefDescription $description -ScriptBlock {
        param($key, $arg)
        Expand-Macros
        if ($func -ne '') {
            [PSConsoleReadLine]::$func($key, $arg)
        }
    }.GetNewClosure() | Out-Null
}

# Toggle history prediction with list view on or off
function ToggleHistoryListView {
    if ((Get-PSReadLineOption).PredictionViewStyle -eq 'ListView') {
        Reset-HistorySearch
        [PSConsoleReadLine]::Insert('')
        return
    }

    # If search was used before list, revert suggestion
    $cmd = Get-PSReadLineBuffer
    if ($cmd -eq $HistoryState.CmdAfter -and $HistoryState.CmdBefore -ne $HistoryState.CmdAfter) {
        [PSConsoleReadLine]::RevertLine()
        [PSConsoleReadLine]::Insert($HistoryState.CmdBefore)
    }

    Set-PSReadLineOption -PredictionSource History
    Set-PSReadLineOption -PredictionViewStyle ListView
    Expand-Macros
    [PSConsoleReadLine]::Insert('')
}

# Search history backwards, but loop if we get to the beginning of history
function HistorySearchBackwardLoop {
    $cmdBefore = Get-PSReadLineBuffer
    if ((Get-PSReadLineOption).PredictionViewStyle -eq 'ListView') {
        ToggleHistoryListView
    }
    if ($HistoryState.CmdBefore -eq '') {
        $HistoryState.CmdBefore = $cmdBefore
    }
    Expand-Macros
    [PSConsoleReadLine]::HistorySearchBackward()
    $cmd = Get-PSReadLineBuffer
    if ($cmd -eq $cmdBefore) {
        [PSConsoleReadLine]::EndOfHistory()
        [PSConsoleReadLine]::HistorySearchBackward()
        $cmd = Get-PSReadLineBuffer
    }
    $HistoryState.CmdAfter = $cmd
}

# Reset history search state to defaults
function Reset-HistorySearch {
    $HistoryState.CmdBefore = ""
    $HistoryState.CmdAfter = ""
    Set-PSReadLineOption -PredictionViewStyle InlineView
    Set-PSReadLineOption -PredictionSource None
}

# Set the prompt, avoid flickering and sluggishness of posh-git default
#  - Don't use git status (branch info is enough)
#  - Return a single string, don't make independent calls to Write-Host
function Prompt {
    Reset-HistorySearch

    $cyan="`e[36m"
    $yellow="`e[33m"
    $plain="`e[0m"

    $prompt = "`n${cyan}$(Get-Location)"
    $branch = (Get-GitBranch)
    if ($branch) {
        $prompt += " ${yellow}(${branch})"
    }

    $symbol = if (Test-Admin) { "#" } else { "$" }
    $prompt += "${plain}`n${symbol} "
    return $prompt
}

# add things to path if available
foreach ($each in (
    "${Env:ProgramW6432}\Beyond Compare 5",
    "${Env:LOCALAPPDATA}\Programs\Beyond Compare 5",
    "${Env:LOCALAPPDATA}\Programs\ILSpy"
    )) {
    if (Test-Path $each) {
        $Env:PATH = "${Env:PATH};$each"
    }
}

# add custom bin dir to PATH
$Env:PATH="${Env:USERPROFILE}\.dot\bin;${Env:PATH}"

# add Git directory to PATH
if (Test-Path "${Env:ProgramW6432}\Git") {
    $GIT_DIR="${Env:ProgramW6432}\Git"
    # at the end to avoid conflicts such as find.exe breaking Windows things
    $Env:PATH="${Env:PATH};$GIT_DIR\bin;$GIT_DIR\mingw64\bin;$GIT_DIR\usr\bin"
    # but make find available as gfind
    Set-Alias gfind "$GIT_DIR\usr\bin\find.exe"
    # and use it interactively as just find
    Set-Macro find gfind
}

# Disable npm update check on Windows. It constantly errors out for me even
# though I keep it up to date.
$Env:NO_UPDATE_NOTIFIER = 'true'

# Disable message about using preview versions of .NET SDK
$Env:SuppressNETCoreSdkPreviewMessage = 'true'

# Load VS developer environment
# Accepts vswhere args to pick which VS to use.
# Uses latest preview by default, latest RTM if Use-RTM was invoked.
function VSEnv {
    Clear-VSEnv
    [string[]]$vars = Get-ChildItem Env: | ForEach-Object { $_.Name }
    $script:_NG_ENV_VARS_BEFORE_VSENV = [HashSet[string]]::new($vars)

    $vswhereArgs = $args
    if ($args.Length -eq 0) {
        $vswhereArgs = $Env:_NG_VSENV_VSWHERE_ARGS.Split(' ', [StringSplitOptions]::RemoveEmptyEntries)
    }

    $installPath = & vswhere @vswhereArgs -property installationPath
    if (!$installPath) {
        return # No matching VS
    }

    # Locate Microsoft.VisualStudio.DevShell.dll module. We can only load one
    # version per process, so always use the absolute latest including previews
    # and hope that it is backwards compatible enough.
    if (!$Env:_NG_VSDEVSHELL_MODULE) {
        $latest = & vswhere -latest -prerelease -property installationPath
        if (!$latest) {
            return # No VS at all
        }
        $module = Join-Path $latest 'Common7\Tools\Microsoft.VisualStudio.DevShell.dll'
        $Env:_NG_VSDEVSHELL_MODULE=$module
    }

    Import-Module $Env:_NG_VSDEVSHELL_MODULE
    Enter-VsDevShell -VsInstallPath $installPath -SkipAutomaticLocation
}

function vswhere {
     $vswhere = 'C:\Program Files (x86)\Microsoft Visual Studio\Installer\vswhere.exe'
     if (Test-Path $vswhere) {
        & $vswhere @args
     }
}

# Since loading VS Environment is slow and not always needed these days, do it
# lazily when one of these commands is first attempted.
foreach ($each in ('csi', 'cl', 'csc', 'dumpbin', 'ildasm', 'link', 'msbuild', 'devenv')) {
    New-Item -Path "function:$each" -Value {
        if (!$Env:VSCMD_VER) {
            VSEnv
        }
        $cmd = if ($each -eq 'devenv') { "$each.com" } else { "$each.exe" }
        & $cmd @args
    }.GetNewClosure() | Out-Null
}

# Clear previous invocation of VSEnv
function Clear-VSEnv {
    if (!$Env:_NG_PATH_BEFORE_VSENV) {
        $Env:_NG_PATH_BEFORE_VSENV = $Env:PATH
        return
    }
    $Env:PATH = $Env:_NG_PATH_BEFORE_VSENV

    if ($script:_NG_ENV_VARS_BEFORE_VSENV) {
        foreach ($var in Get-ChildItem Env: | ForEach-Object { $_.Name }) {
            if (!$script:_NG_ENV_VARS_BEFORE_VSENV.Contains($var)) {
                Remove-Item Env:\$var
            }
        }
    }
    $script:_NG_ENV_VARS_BEFORE_VSENV = $null
}

function Use-Previews {
    Use-VS-Preview
    Use-Code-Insiders
}

function Use-RTM {
    Use-VS-RTM
    Use-Code-RTM
}

function Use-VS-Preview {
    Clear-VSEnv
    $Env:_NG_VSENV_VSWHERE_ARGS="-latest -prerelease"

}

function Use-VS-RTM {
    Clear-VSEnv
    $Env:_NG_VSENV_VSWHERE_ARGS="-latest"
}

function Use-Code-Insiders {
    if (Test-Command code-insiders) {
        Set-Alias -Scope Global code code-insiders
    }
}

function Use-Code-RTM {
    if (Test-Path Alias:\code) {
        Remove-Alias -Force code
    }
}

# Use VS and VS Code previews by default
# Commands above can be used to switch
Use-Previews

# Mimic unix xargs in a way that works better on Windows and PowerShell
# Process one line at a time, allowing spaces, and don't munge backslashes
function xargs($command) {
    process { & $command @args $_.Trim() }
}

# List all powershell commands when which or where is called interactively.
# Morally equivalent to Windows where.exe, or bash type -a
Set-Macro where pswhere
Set-Macro which pswhere
function pswhere { Get-Command -All @args | Format-Table -AutoSize -Wrap }

# Mimic unix time command. Measure a command without capturing its output.
function time {
    $cmd = Get-PSReadLineBuffer
    $cmd = $cmd -replace '^time\s', ''

    if ($cmd -match '^time($|\s)')
    {
        Write-Error "cannot time time itself."
        return
    }

    $measured = (Measure-Command { Invoke-Expression $cmd | Out-Default }.GetNewClosure()).TotalMilliseconds
    [PSCustomObject]@{Time = "$measured ms"}
}

# Use cmd dir/rd/del when given cmd-like arguments interactively, otherwise use
# Keeps muscle memory intact and makes and also provides terse access to faster
# /s recursion than PowerShell equivalent.
foreach ($each in ('dir', 'rd', 'del')) {
    Set-Macro $each {
        param ($command, $arg)
        if (Test-CmdArg $arg) {
            return "cmd /c $command"
        }
    }
}

# Use UNIX ls when given no arguments or unix-like arguments interactively
Set-Macro ls {
    param($command, $arg)
    if (!$arg -or (Test-UnixArg $arg) -or ($arg -eq '-al')) {
        return 'gls'
    }
}

# Mimic cmd set
Set-Macro set {
    param ($command)

    # If we have one argument with an equal sign, set environment variable
    # NOTE: This can't be done by shelling out to cmd as it will get its own process
    if ($args.Length -eq 1) {
        $arg = $args[0]
        $equalIndex = $arg.IndexOf('=')
        if ($equalIndex -cge 0) {
            $variable = $arg.Substring(0, $equalIndex)
            $value = $arg.Substring($equalIndex + 1)

            # Do the replacement ourselves as return value doesn't overwrite arg, only command
            [PSConsoleReadLine]::RevertLine()
            if ($value -eq '') {
                [PSConsoleReadLine]::Insert("rm Env:\$variable")
            } else {
                [PSConsoleReadLine]::Insert("`$Env:$variable = '$value'")
            }
            [PSConsoleReadLine]::EndOfLine()
            return $null
        }
    }

    # Otherwise, if we have one or no arguments, use cmd set
    # No arguments will list all environment variables
    # One arugment will list all environment variables with the given prefix
    if ($args.Length -lt 2) {
        return 'cmd /c set'
    }
}

# Use UNIX diff when diff is called interactively
Set-Macro diff diff.exe

function .. { Set-Location .. }
function du { du.exe -h @args }
function df { df.exe -h @args }
function emacs { emacsclient.cmd -n @args }
function ll { gls -l @args }
function ms { emacsclient.cmd `-e '"(progn (magit-status) (raise-frame))"' }
function ver { cmd /c ver }

function gls { 
    # Hide well-known windows hidden files
    ls.exe --group-directories-first --color -h -F `
        --ignore=*fil*.sys `
        --ignore=?Recycle.Bin `
        --ignore=?RECYCLE.BIN `
        --ignore=?WinREAgent `
        --ignore=?SysReset `
        --ignore=Application?Data `
        --ignore=bootmgr `
        --ignore=BOOTNXT `
        --ignore=Config.Msi `
        --ignore=Documents?and?Settings `
        --ignore=DumpStack.log.tmp `
        --ignore=Local?Settings `
        --ignore=My?Documents `
        --ignore=ntuser.* `
        --ignore=NTUSER.* `
        --ignore=OneDriveTemp `
        --ignore=Recovery `
        --ignore=system.sav `
        --ignore=System?Volume?Information `
        --ignore=?WINRE_BACKUP_PARTITION.MARKER `
        --ignore=desktop.ini `
        @args 
}


Set-Alias e emacs
Set-Alias h history
Set-Alias n notepad
Set-Alias traceroute tracert
Set-Alias vi vim

if (Test-Command code) {
    Set-Alias n code
    Set-Alias notepad code
}

# Add completion for dotnet
Register-ArgumentCompleter -Native -CommandName dotnet -ScriptBlock {
    param($wordToComplete, $commandAst, $cursorPosition)
        dotnet complete --position $cursorPosition "$commandAst" | ForEach-Object {
            [CompletionResult]::new($_, $_, 'ParameterValue', $_)
        }
}

Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineOption -BellStyle Visual
Set-PSReadLineOption -PredictionSource None
Set-PSReadLineOption -HistorySearchCursorMovesToEnd

Set-PSReadLineKeyHandlerWithMacroExpansion Enter AcceptLine
Set-PSReadLineKeyHandlerWithMacroExpansion Tab  TabCompleteNext
Set-PSReadLineKeyHandlerWithMacroExpansion Shift+Tab TabCompletePrevious
Set-PSReadLineKeyHandlerWithMacroExpansion F6 ''
Set-PSReadLineKeyHandler -Key F7 -BriefDescription ToggleHistoryListView -ScriptBlock { ToggleHistoryListView }
Set-PSReadLineKeyHandler -Key Ctrl+r -BriefDescription ToggleHistoryListView -ScriptBlock { ToggleHistoryListView }
Set-PSReadLineKeyHandler -Key F8 -BriefDescription HistorySearchBackwardLoop -ScriptBlock { HistorySearchBackwardLoop }
Remove-PSReadLineKeyHandler -Key Ctrl+s