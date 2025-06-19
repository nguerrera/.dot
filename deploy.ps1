# Set HOME environment variable permanently. emacs uses APPDATA otherwise. Note
# that Environment.SetEnvironmentVariable can be very slow while notifying
# windows of the change, so we edit the registry directly.
Write-Output "Setting HOME environment variable to $env:USERPROFILE..."
$env:HOME = $env:USERPROFILE
Set-ItemProperty -Path "HKCU:\Environment" -Name "HOME" -Value $env:HOME

# Git Config
$gitConfigPath = "$PSScriptRoot\etc\win.gitconfig"
Write-Output "Configuring git to include use $gitConfigPath.."
git config --global include.path $gitConfigPath

# Disable safe directories so Windows git can operate on WSL mounts. For some
# reason, this has to be in the top-level config file. And it also does not
# support wildcard other than single '*' to match absolutely everything. :(
Write-Output "Configuring git to disable safe directories..."
git config --global safe.directory "*"

# Dot files
Get-ChildItem -Path "$PSScriptRoot\.*"  | ForEach-Object {
    if (-not $_.Name.StartsWith(".git")) {
        $link = Join-Path -Path $env:HOME -ChildPath $_.Name
        $target = $_.FullName
        if (Test-Path $link) {
           Write-Warning "'$link' already exists, not overwriting."
        } else {
            $itemType = $_.PSIsContainer ? "Junction" : "SymbolicLink"
            Write-Output "$_.Name -> $target"
            New-Item -ItemType $itemType -Path $link -Target $_ | Out-Null
        }
    }
}

# Windows Terminal
$terminalPaths = @(
    "$env:LOCALAPPDATA\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState",
    "$env:LOCALAPPDATA\Packages\Microsoft.WindowsTerminalPreview_8wekyb3d8bbwe\LocalState"
)
foreach ($terminalPath in $terminalPaths) {
    if (-not (Test-Path $terminalPath)) {
        continue
    }
    if (Test-Path "$terminalPath\settings.json") {
        Write-Warning "'$terminalPath\settings.json' already exists, not overwriting."
    } else {
        Write-Output "$terminalPath\settings.json -> $PSScriptRoot\etc\settings.json"
        New-Item -ItemType SymbolicLink -Path "$terminalPath\settings.json" -Target "$PSScriptRoot\etc\settings.json" | Out-Null
    }
}

# PowerShell profile
#
# Can't use symlink in Documents folder because OneDrive backup breaks. Emit a
# one line powershell profile that imports profile.ps1 from here instead.
$docs = [Environment]::GetFolderPath('Personal')
$psFolder = Join-Path -Path $docs -ChildPath "PowerShell"
New-Item -ItemType Directory -Path $psFolder -Force | Out-Null
$psProfile = Join-Path -Path $psFolder -ChildPath "Microsoft.PowerShell_profile.ps1"

if (Test-Path -Path $psProfile) {
    Write-Warning "'$psProfile' already exists, not overwriting"
} else {
    Write-Output "$psProfile -> $PSScriptRoot\etc\profile.ps1"
    Set-Content -Path $psProfile -Value ". `"$PSScriptRoot\etc\profile.ps1`""
}