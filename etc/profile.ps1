using namespace System.Management.Automation
using namespace System.Management.Automation.Language
using namespace System.Security.Principal
using namespace Microsoft.PowerShell

Import-Module posh-git

Set-PSReadLineOption -EditMode Emacs -BellStyle Visual
Set-PSReadLineOption -PredictionSource None

Set-PSReadLineKeyHandler -Key Tab -Function TabCompleteNext
Set-PSReadLineKeyHandler -Key Shift+Tab -Function TabCompletePrevious
Set-PSReadLineKeyHandler -Key F6 -BriefDescription ExpandMacros { Expand-Macros }
Set-PSReadLineKeyHandler -Key F7 -BriefDescription SwitchHistoryList -ScriptBlock { Switch-HistoryList }
Set-PSReadLineKeyHandler -Key F8 -BriefDescription UseHistorySuggestion -ScriptBlock { Use-HistorySuggestion }

Set-PSReadLineKeyHandler -Key Enter -BriefDescription 'ExpandMacrosAndAcceptLine' -ScriptBlock {
    param($key, $arg)
    Expand-Macros
    [PSConsoleReadLine]::AcceptLine($key, $arg)
}

# Clean up after prior invocations
if (!$Env:PathBeforeProfile) {
    $Env:PathBeforeProfile = $Env:PATH
}
$Env:PATH = $Env:PathBeforeProfile

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
    if (!$arg || $arg.Length -lt 2 || $arg[0] -ne '/') {
        return $false;
    }

    # special case for dir /s/b without space between switches
    if ($arg -eq "/s/b") {
        return $true;
    }

    # /x
    if ($arg.Length -eq 2) {
        return $true;
    }

    # /x:y
    if ($arg.Length -eq 4) {
        return $arg[2] -eq ':'
    }

    return $false;
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
    $command = $extent.Text
    $replacement = $Macros[$command]
    if (!$replacement) {
        return
    }

    if ($tokens.Length -eq 2) {
        $arguments = @();
    } else {
        $arguments = [string[]]($tokens[1 .. ($tokens.Length - 2)] | ForEach-Object { $_.Extent.Text })
    }
    if ($replacement -is [scriptblock]) {
        $replacement = & $replacement $command @arguments
    }

    if ($replacement) {
        [PSConsoleReadLine]::Replace($extent.StartOffset, $extent.EndOffset - $extent.StartOffset, $replacement)
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
    $buffer = $null;
    $cursor = $null;
    [PSConsoleReadLine]::GetBufferState([ref]$buffer, [ref]$cursor);
    return $buffer;
}

$HistoryState = @{ 
    CmdBefore = ""; 
    CmdAfter = ""; 
}

# Make Clear-History actually clear everything
function Clear-History {
    [PSConsoleReadLine]::ClearHistory()
    Remove-Item (Get-PSReadLineOption).HistorySavePath
    & (Get-Command -CommandType Cmdlet Clear-History)
}

# Bound to F8: Temporarily enables prediction and accepts the first suggestion
function Use-HistorySuggestion {
    $cmd = Get-PSReadLineBuffer
    if ($cmd -eq $HistoryState.CmdAfter) {
        return
    }

    $HistoryState.CmdBefore = Get-PSReadLineBuffer;
    Set-PSReadLineOption -PredictionSource History
    Set-PSReadLineOption -PredictionViewStyle InlineView
    Expand-Macros
    [PSConsoleReadLine]::Insert('')
    [PSConsoleReadLine]::AcceptSuggestion()
    Set-PSReadLineOption -PredictionSource None
    $HistoryState.CmdAfter = Get-PSReadLineBuffer; 
}

# Bound to F7: Toggle prediction with list view on or off
function Switch-HistoryList {
    if ((Get-PSReadLineOption).PredictionViewStyle -eq 'ListView') {
        Reset-HistorySuggestion
        [PSConsoleReadLine]::Insert('')
        return
    }

    # If suggestion was used before list, revert suggestion
    $cmd = Get-PSReadLineBuffer
    if ($cmd -eq $HistoryState.CmdAfter -and $HistoryState.CmdBefore -ne $HistoryState.CmdAfter) {
        [PSConsoleReadLine]::RevertLine()
        [PSConsoleReadLine]::Insert($HistoryState.CmdBefore);
    }

    Set-PSReadLineOption -PredictionSource History
    Set-PSReadLineOption -PredictionViewStyle ListView
    Expand-Macros
    [PSConsoleReadLine]::Insert('')
}

function Reset-HistorySuggestion {
    $HistoryState.CmdBefore = "";
    $HistoryState.CmdAfter = "";
    Set-PSReadLineOption -PredictionViewStyle InlineView
    Set-PSReadLineOption -PredictionSource None
}

# Set the prompt, avoid flickering and sluggishness of posh-git default
#  - Don't use git status (branch info is enough)
#  - Return a single string, don't make independent calls to Write-Host
function Prompt {
    Reset-HistorySuggestion

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
    "$Env:LOCALAPPDATA\Programs\ILSpy"
    )) {
    if (Test-Path $each) {
        $Env:PATH = "${Env:PATH};$each";
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
$Env:NO_UPDATE_NOTIFIER='true'

# Load VS developer environment
#
# Accepts vswhere args to pick which VS to use, defaults to -latest -prerelease
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

# Since loading VS Environment is slow and not always needed these days, do it
# lazily when one of these commands is first attempted.
foreach ($each in ('csi', 'cl', 'csc', 'dumpbin', 'ildasm', 'link', 'msbuild', 'devenv')) {
    New-Item -Path "function:$each" -Value {
        if (!(Test-Path Env:VSCMD_VER)) {
            VSEnv
        }
        $cmd = if ($each -eq 'devenv') { "$each.com" } else { "$each.exe" }
        & $cmd @args
    }.GetNewClosure() | Out-Null
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
    $Env:EXTERNAL_INCLUDE = ''
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
    $Env:VCPKG_ROOT = ''
    $Env:VCToolsInstallDir = ''
    $Env:VCToolsRedistDir = ''
    $Env:VCToolsVersion = ''
    $Env:VisualStudioVersion = ''
    $Env:VS160COMNTOOLS = ''
    $Env:VS170COMNTOOLS = ''
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
function xargs($command) {
    process { & $command @args $_.Trim() }
}

# sudo for versions of Windows without sudo.exe
function _sudo($command) {
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
if (!(Test-Command sudo.exe)) {
    Set-Alias sudo _sudo
}

# List all powershell commands when which or where is called interactively.
# Morally equivalent to Windows where.exe, or bash type -a
Set-Macro where pswhere
Set-Macro which pswhere
function pswhere { Get-Command -All @args | Format-Table -AutoSize -Wrap }

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

# Get git branch, ported from posh-git, and changed to avoid slow shelling
# out to git as much as possible
function Get-GitBranch($gitDir = $(Get-GitDirectory)) {
    if (!$gitDir) { return $null }
    $r = ''; $b = ''; $c = ''
    $step = ''; $total = ''
    if (Test-Path $gitDir/rebase-merge) {
        if (Test-Path $gitDir/rebase-merge/interactive) {
            $r = '|REBASE-i'
        }
        else {
            $r = '|REBASE-m'
        }
        $b = "$(Get-Content $gitDir/rebase-merge/head-name)"
        $step = "$(Get-Content $gitDir/rebase-merge/msgnum)"
        $total = "$(Get-Content $gitDir/rebase-merge/end)"
    }
    else {
        if (Test-Path $gitDir/rebase-apply) {
            $step = "$(Get-Content $gitDir/rebase-apply/next)"
            $total = "$(Get-Content $gitDir/rebase-apply/last)"

            if (Test-Path $gitDir/rebase-apply/rebasing) {
                $r = '|REBASE'
            }
            elseif (Test-Path $gitDir/rebase-apply/applying) {
                $r = '|AM'
            }
            else {
                $r = '|AM/REBASE'
            }
        }
        elseif (Test-Path $gitDir/MERGE_HEAD) {
            $r = '|MERGING'
        }
        elseif (Test-Path $gitDir/CHERRY_PICK_HEAD) {
            $r = '|CHERRY-PICKING'
        }
        elseif (Test-Path $gitDir/REVERT_HEAD) {
            $r = '|REVERTING'
        }
        elseif (Test-Path $gitDir/BISECT_LOG) {
            $r = '|BISECTING'
        }

        $b = & {
            $ref = $null
            if (Test-Path $gitDir/HEAD) {
                $ref = Get-Content $gitDir/HEAD 2>$null
            }
            else {
                $ref = git --no-optional-locks rev-parse HEAD 2>$null
            }

            if ($ref -match 'ref: (?<ref>.+)') {
                return $Matches['ref']
            }
            elseif ($ref -and $ref.Length -ge 7) {
                return $ref.Substring(0,7)+'...'
            }
            else {
                return 'unknown'
            }
        }
    }

    if ($step -and $total) {
        $r += " $step/$total"
    }

    return "$c$($b -replace 'refs/heads/','')$r"
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
            [PSConsoleReadLine]::Insert("`$Env:$variable = '$value'")
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

# Prefer VS Code Insiders
if (Test-Command code-insiders) {
    Set-Alias code code-insiders
}

if (Test-Command code) {
    Set-Alias n code
    Set-Alias notepad code
}
