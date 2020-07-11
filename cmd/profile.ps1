Import-Module posh-git

Set-PSReadLineOption -EditMode Emacs -BellStyle Visual
Set-PSReadlineKeyHandler -Key Tab -Function TabCompleteNext
Set-PSReadlineKeyHandler -Key Shift+Tab -Function TabCompletePrevious

# Clean up after prior invocations
if (!$Env:PathBeforeProfile) {
    $Env:PathBeforeProfile = $Env:PATH
}
$Env:PATH = $Env:PathBeforeProfile

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
    Set-Alias gfind "$GIT_DIR\usr\bin\find.exe"
}

# use hub as alias for git if available
if (Test-Path "${Env:UserProfile}\OneDrive\Tools\Hub\bin") {
    Set-Alias git "${Env:UserProfile}\OneDrive\Tools\Hub\bin\hub.exe"
}

# Check if a given command is available
function Test-Command {
    param($command)
    Get-Command -ErrorAction SilentlyContinue $Command | Out-Null
    $?
}

# Check if we're running as admin
function Test-Admin {
    $principal = [Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]'Administrator')
}

# Check if given string looks like a unix arg (--something or -x)
function Test-UnixArg {
    param([string]$arg)
    Write-Host $arg
    return $arg.StartsWith('--') -or (($arg.Length -eq 2) -and $arg[0] -eq '-')
}

# Check if the given string looks like a cmd arg (/x)
function Test-CmdArg {
    param([string]$arg)
    return ($arg.Length -eq 2) -and ($arg[0] -eq '/')
}

# Remove an alias no matter how hard powershell resists
function Remove-Alias-WithPrejudice {
    param($command)
    while (Test-Path "Alias:$command") {
        Remove-Alias -Force $command
    }
}

# Set the prompt
# NOTE: Avoid flickering and sluggishness of posh-git prompt:
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

# Same as Where-Object except when called with no pipeline input and no switches,
# then it behaves like bash type or windows where.exe.
Remove-Alias-WithPrejudice where
function where {
    process {
        if ($_ -or ($args.Length -ne 1) -or ($args[0].StartsWith('-'))) {
            $_ | Where-Object @args
        } else {
            Get-Command -All @args
        }
    }
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

# Overload ls to GNU ls when given unix-like arguments
# Otherwise, behave as Get-ChildItem as usual
Remove-Alias-WithPrejudice ls
function ls {
    process {
        if ($_ -or ($args.Length -eq 0) -or !(Test-UnixArg $args[0])) {
            $_ | Get-ChildItem @args
        } else {
            gls @args
        }
    }
}

# Overload dir to cmd dir when given cmd-like arguments
# Otherwise, behave as Get-ChildItem as usual
Remove-Alias-WithPrejudice dir
function dir {
    process {
        if ($_ -or ($args.Length -eq 0) -or !(Test-CmdArg $args[0])) {
            $_ | Get-ChildItem @args
        } else {
            cmd /c dir /o:gn @args
        }
    }
}

# Overload rd to cmd rd when given cmd-like arguments
# Otherwise, behave as Remove-Item as usual
Remove-Alias-WithPrejudice rd
function rd {
    process {
        if ($_ -or ($args.Length -eq 0) -or !(Test-CmdArg $args[0])) {
            $_ | Remove-Item @args
        } else {
            /c rd @args
        }
    }
}

# Mimic cmd set when given
#  no args: list all environment variable
#  on arg with no '=': list all environment variables with given prefix
#  1 arg with '=': set environment variable
# Otherwise, behave as Set-Variable as usual
Remove-Alias-WithPrejudice set
function set {
    if ($_ -or ($args.Length -gt 1)) {
        $_ | Set-Variable @args
    } else {
        $equalIndex = $args[0].IndexOf('=')
        if ($equalIndex -cge 0) {
            $variable = $args[0].Substring(0, $equalIndex)
            $value = $args[0].Substring($equalIndex + 1)
            New-Item "Env:\$variable" -Value $value
        } else {
            cmd /c set @args
        }
    }
}

# Mimic unix xargs in a way that works better on Windows and PowerShell
# Process one line at a time, allowing spaces, don't munge backslashes
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

# alias-like functions
function .. { Set-Location .. }
function /c { cmd /c @args }
function du { du.exe -h @args }
function df { df.exe -h @args }
function emacs { emacsclient.cmd -n @args }
function ms { emacsclient.cmd `-e '"(progn (magit-status) (raise-frame))"' }
function which { Get-Command -All @args }
function gls { ls.exe --color -h -F --ignore="ntuser.*" --ignore="NTUSER.*" --ignore="*fil*.sys" @args }
function ver { cmd /c ver }

# aliases
Set-Alias h history
Set-Alias e emacs
Set-Alias vi vim
Set-Alias n notepad 

# Prefer VS Code Insiders
if (Test-Command code-insiders) {
    Set-Alias code code-insiders
}

if (Test-Command code) {
    Set-Alias n code
    Set-Alias notepad code
}