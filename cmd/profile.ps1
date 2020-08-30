using namespace System.Management.Automation
using namespace System.Management.Automation.Language
using namespace Microsoft.PowerShell

Import-Module posh-git
Set-PSReadLineOption -EditMode Emacs -BellStyle Visual
Set-PSReadlineKeyHandler -Key Tab -Function TabCompleteNext
Set-PSReadlineKeyHandler -Key Shift+Tab -Function TabCompletePrevious

# Clean up after prior invocations
if (!$Env:PathBeforeProfile) {
    $Env:PathBeforeProfile = $Env:PATH
}
$Env:PATH = $Env:PathBeforeProfile

# Check if a given command is available
function Test-Command {
    param($command)
    Get-Command -ErrorAction SilentlyContinue $Command | Out-Null
    return $?
}

# Check if we're running as admin
function Test-Admin {
    $principal = [Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]'Administrator')
}

# Check if given string looks like a unix arg (--something or -x)
function Test-UnixArg {
    param([string]$arg)
    return $arg -and ($arg.StartsWith('--') -or (($arg.Length -eq 2) -and $arg[0] -eq '-'))
}

# Check if the given string looks like a cmd arg (/x)
function Test-CmdArg {
    param([string]$arg)
    return ($arg -and $arg.Length -eq 2 -and $arg[0] -eq '/')
}

# Register a macro to replace the given command when it appears as the first
# token of command line is $command, replace it with $replacement.
#
# $replacement can be a string or a scriptblock taking three args: command being
# replaced, first argument to command, and total number of arguments that
# returns replacement dynamically, or opts out of replacing by returning $null.
#
# TODO: It would be more elegant if the replacment block took an array of args
#       instead of first arg and arg count.
#
# These macros are invisible to scripts or in position beyond the start of the
# command line. This allows us to customize built-in commands without breaking
# anything.
#
$Macros = @{}
function Set-Macro {
    param ([string]$command, $replacement)
    $Macros[$command] = $replacement
}

# Replace command line according to registered macro
function Expand-Macros {
    $tokens = Get-PSReadLineTokens
    if ($tokens.Count -eq 0) {
        return
    }

    $extent = $tokens[0].Extent
    $command = $extent.Text
    $replacement = $Macros[$command]
    if (!$replacement) {
        return
    }

    # argument count minus command itself and end token
    $count = $tokens.Length - 2
    $arg = $null
    if ($count -gt 0) {
        $arg = $tokens[1].Extent.Text
    }

    if ($replacement -is [scriptblock]) {
        $replacement = & $replacement $command $arg $count
    }

    if ($replacement) {
        [PSConsoleReadLine]::Replace($extent.StartOffset, $extent.EndOffset - $extent.StartOffset, $replacement)
    }
}

# Helper to get just the tokens from PSReadLine buffer state
function Get-PSReadLineTokens {
    $ast = $null
    $tokens = $null
    $errors = $null
    $cursor = $null
    [PSConsoleReadLine]::GetBufferState([ref]$ast, [ref]$tokens, [ref]$errors, [ref]$cursor)
    return $tokens
}

# On enter key press, expand macros and accept line
Set-PSReadLineKeyHandler -Key Enter -BriefDescription 'ExpandMacrosAndAcceptLine' -ScriptBlock {
    param($key, $arg)
    Expand-Macros
    [PSConsoleReadLine]::AcceptLine($key, $arg)
}

# Set the prompt, avoid flickering and sluggishness of posh-git default
#  - Don't use git status (branch info is enough)
#  - Return a single string, don't make independent calls to Write-Host
function prompt {
    $cyan="`e[36m"
    $yellow="`e[33m"
    $plain="`e[0m"

    $prompt = "`n${cyan}$(Get-Location)"
    $branch = Get-GitBranch
    if ($branch) {
        $prompt += " ${yellow}(${branch})"
    }

    $prompt += "${plain}`n> "
    return $prompt
}

# add beyond compare to the path
if (Test-Path "${Env:ProgramW6432}\Beyond Compare 4") {
    $Env:PATH="${Env:PATH};${Env:ProgramW6432}\Beyond Compare 4"
}

# add this directory to PATH
$Env:PATH="${Env:USERPROFILE}\.dot\cmd;${Env:PATH}"

# add Git directory to PATH
if (Test-Path "${Env:ProgramW6432}\Git") {
    $GIT_DIR="${Env:ProgramW6432}\Git"
    # at the end to avoid conflicts such as find.exe breaking Windows things
    $Env:PATH="${Env:PATH};$GIT_DIR\mingw64\bin;$GIT_DIR\usr\bin"
    # but make find available as gfind
    Set-Alias gfind "$GIT_DIR\usr\bin\find.exe"
    # and use it interactively as just find
    Set-Macro find gfind
}

# use hub as alias for git if available
if (Test-Path "${Env:UserProfile}\OneDrive\Tools\Hub\bin") {
    Set-Alias git "${Env:UserProfile}\OneDrive\Tools\Hub\bin\hub.exe"
}

# Disable npm update check on Windows. It constantly errors out for me even
# though I keep it up to date.
$Env:NO_UPDATE_NOTIFIER='true'

# Load VS developer environment
#
# Accepts vswhere args to pick which VS to use, defaults to -latest -prerelease
#
# NOTE: This is slow and I don't need it so much these days, so tuck it behind a
# helper function that can be invoked when needed.
function VSEnv {
    Clear-VSEnv

    $vswhere = 'C:\Program Files (x86)\Microsoft Visual Studio\Installer\vswhere.exe'
    $vswhereArgs = $args
    if ($args.Length -eq 0) {
        $vswhereArgs = @('-latest', '-prerelease')
    }

    if (Test-Path $vswhere) {
        $installPath = & $vswhere @vswhereArgs -property installationPath
        if ($installPath) {
            Import-Module (Join-Path $installPath 'Common7\Tools\Microsoft.VisualStudio.DevShell.dll')
            Enter-VsDevShell -VsInstallPath $installPath -SkipAutomaticLocation
        }
    }
}

# Clear previous invocation of VSEnv
function Clear-VSEnv {
    if (!$Env:PathBeforeVSEnv) {
        $Env:PathBeforeVSEnv = $Env:PATH
        return
    }

    $Env:Path = $Env:PathBeforeVSEnv
    $Env:CommandPromptType = ''
    $Env:DevEnvDir = ''
    $Env:ExtensionSdkDir = ''
    $Env:Framework40Version = ''
    $Env:FrameworkDir = ''
    $Env:FrameworkDir32 = ''
    $Env:FrameworkVersion = ''
    $Env:FrameworkVersion32 = ''
    $Env:FSHARPINSTALLDIR = ''
    $Env:INCLUDE = ''
    $Env:LIB = ''
    $Env:LIBPATH = ''
    $Env:NETFXSDKDir = ''
    $Env:UCRTVersion = ''
    $Env:UniversalCRTSdkDir = ''
    $Env:VCIDEInstallDir = ''
    $Env:VCINSTALLDIR = ''
    $Env:VCToolsInstallDir = ''
    $Env:VCToolsRedistDir = ''
    $Env:VCToolsVersion = ''
    $Env:VisualStudioVersion = ''
    $Env:VS160COMNTOOLS = ''
    $Env:VSCMD_ARG_app_plat = ''
    $Env:VSCMD_ARG_HOST_ARCH = ''
    $Env:VSCMD_ARG_TGT_ARCH = ''
    $Env:VSCMD_VER = ''
    $Env:VSINSTALLDIR = ''
    $Env:WindowsLibPath = ''
    $Env:WindowsSdkBinPath = ''
    $Env:WindowsSdkDir = ''
    $Env:WindowsSDKLibVersion = ''
    $Env:WindowsSdkVerBinPath = ''
    $Env:WindowsSDKVersion = ''
    $Env:WindowsSDK_ExecutablePath_x64 = ''
    $Env:WindowsSDK_ExecutablePath_x86 = ''
    $Env:__DOTNET_ADD_32BIT = ''
    $Env:__DOTNET_PREFERRED_BITNESS = ''
    $Env:__VSCMD_PREINIT_PATH = ''
    $Env:__VSCMD_script_err_count = ''
}

# NOTE: This intentionally overrides the tgit from posh-git, which
# doesn't do /path for you and doesn't default to repo root.
function tgit {
    param (
        [Parameter(Mandatory=$true, Position=0)]
        $command,

        [Parameter(Position=1)]
        $path,

        [Parameter(ValueFromRemainingArguments=$true)]
        $args
        )

    if (!$path) {
        $path=git rev-parse --show-toplevel
        if ($LASTEXITCODE -ne 0) {
            return
        }
    }

    & $Global:TortoiseGitSettings.TortoiseGitPath /command:$command /path:$path @args
}

# Mimic unix xargs in a way that works better on Windows and PowerShell
# Process one line at a time, allowing spaces, and don't munge backslashes
function xargs {
    param($command)
    process { & $command @args $_ }
}

# Mimic unix sudo
function sudo {
    param ($command)
    if (Test-Admin) {
        & $command @args
    } else {
        $argumentList = $null
        if ($args) {
            $quotedArgs = $args | ForEach-Object { if ($_.Contains(' ')) { "`"$_`"" } else { $_ } }
            $argumentList = [String]::Join(' ', $quotedArgs)
        }
        Start-Process -Verb RunAs -FilePath $command -ArgumentList $argumentList -WorkingDirectory (Get-Location)
    }
}

# List all powershell commands when which or where is called interactively.
# Morally equivalent to Windows where.exe, or bash type -a
Set-Macro where 'gcm -all'
Set-Macro which 'gcm -all'

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
    param ($command, $arg, $count)

    # If we have one argument with and equal sign, set environment variable
    # NOTE: This can't be done by shelling to cmd as it will get its own process
    if ($count -eq 1) {
        $equalIndex = $arg.IndexOf('=')
        if ($equalIndex -cge 0) {
            $variable = $arg.Substring(0, $equalIndex)
            $value = $arg.Substring($equalIndex + 1)

            # Do the replacement ourselves as return value doesn't overwrite arg, only command
            [PSConsoleReadLine]::RevertLine()
            [PSConsoleReadLine]::Insert("`$Env:$variable = '$value'")
            return $null
        }
    }

    # Otherwise, if we have one or no arguments, use cmd set
    # No arguments will list all environment variables
    # One arugment will list all environment variables with the given prefix
    if ($count -lt 2) {
        return 'cmd /c set'
    }
}

function .. { Set-Location .. }
function du { du.exe -h @args }
function df { df.exe -h @args }
function emacs { emacsclient.cmd -n @args }
function gls { ls.exe --color -h -F --ignore="ntuser.*" --ignore="NTUSER.*" --ignore="*fil*.sys" @args }
function ll { gls -l @args }
function ms { emacsclient.cmd `-e '"(progn (magit-status) (raise-frame))"' }
function ver { cmd /c ver }

Set-Alias e emacs
Set-Alias h history
Set-Alias n notepad
Set-Alias traceroute tracert
Set-Alias vi vim

# Prefer VS Code Insiders
if (Test-Command code-insiders) {
    Set-Alias code code-insiders
}

if (Test-Command code) {
    Set-Alias n code
    Set-Alias notepad code
}
