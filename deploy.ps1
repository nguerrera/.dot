# Set HOME environment variable permanently. emacs uses APPDATA otherwise. Note
# that Environment.SetEnvironmentVariable can be very slow while notifying
# windows of the change, so we edit the registry directly.
Write-Output "Setting HOME environment variable to $env:USERPROFILE..."
$env:HOME = $env:USERPROFILE
Set-ItemProperty -Path "HKCU:\Environment" -Name "HOME" -Value $env:HOME

# Git Config
$gitConfigPath = "$PSScriptRoot\etc\win.gitconfig"
Write-Output "Configuring git to use $gitConfigPath.."
git config --global include.path $gitConfigPath

# Disable safe directories so Windows git can operate on WSL mounts. For some
# reason, this has to be in the top-level config file. And it also does not
# support wildcard other than single '*' to match absolutely everything. :(
Write-Output "Configuring git to disable safe directories..."
git config --global safe.directory "*"

# Back up an obstruction at $path to a numbered .bak sibling, then remove it.
function Backup($path) {
    $bak = "$path.bak"
    $n = 1
    while (Test-Path $bak) {
        $bak = "$path.bak.$n"
        $n++
    }
    Write-Output "Backing up $path -> $bak"
    Move-Item -Path $path -Destination $bak
}

# Create or repair a symbolic link or junction at $link pointing to $target.
# An existing link that already points to $target is left alone. Any other
# obstruction is backed up first so that re-running converges.
function Deploy-Link($link, $target, $itemType) {
    $existing = Get-Item -Path $link -Force -ErrorAction SilentlyContinue
    if ($existing) {
        if ($existing.LinkType -in @("SymbolicLink", "Junction") -and
            $existing.Target -eq $target) {
            return  # already correct
        }
        Backup $link
    }
    Write-Output "$link -> $target"
    New-Item -ItemType $itemType -Path $link -Target $target | Out-Null
}

# Dot files
Get-ChildItem -Path "$PSScriptRoot\.*"  | ForEach-Object {
    # ~/.claude holds the Claude CLI's credentials, transcripts and caches.
    # A .claude directory here would be linked over the top of them, backing
    # the live one out of the way.
    if (-not $_.Name.StartsWith(".git") -and $_.Name -ne ".claude") {
        $link = Join-Path -Path $env:HOME -ChildPath $_.Name
        $target = $_.FullName
        $itemType = $_.PSIsContainer ? "Junction" : "SymbolicLink"
        Deploy-Link $link $target $itemType
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
    Deploy-Link "$terminalPath\settings.json" "$PSScriptRoot\etc\settings.json" "SymbolicLink"
}

# PowerShell profile
#
# Can't use symlink in Documents folder because OneDrive backup breaks. Emit a
# one line powershell profile that imports profile.ps1 from here instead.
$docs = [Environment]::GetFolderPath('Personal')
$psFolder = Join-Path -Path $docs -ChildPath "PowerShell"
New-Item -ItemType Directory -Path $psFolder -Force | Out-Null
$psProfile = Join-Path -Path $psFolder -ChildPath "Microsoft.PowerShell_profile.ps1"
$psContent = ". `"$PSScriptRoot\etc\profile.ps1`""

if (Test-Path -Path $psProfile) {
    if ((Get-Content -Raw -Path $psProfile).Trim() -ne $psContent.Trim()) {
        Backup $psProfile
        Write-Output "$psProfile -> $PSScriptRoot\etc\profile.ps1"
        Set-Content -Path $psProfile -Value $psContent
    }
} else {
    Write-Output "$psProfile -> $PSScriptRoot\etc\profile.ps1"
    Set-Content -Path $psProfile -Value $psContent
}