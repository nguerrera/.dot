# Run this from a conhost window (plain cmd), not Windows Terminal: the
# terminal holds its settings.json open, and the file cannot be replaced from
# under it.

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

# Move an obstruction out of the way to <path>.bak, numbered when taken, the
# same way deploy does.
function Backup-Item([string] $path) {
    $bak = "$path.bak"
    $n = 1
    while (Test-Path -LiteralPath $bak) {
        $bak = "$path.bak.$n"
        $n++
    }
    Write-Output "$path backed up to $bak"
    Move-Item -LiteralPath $path -Destination $bak
}

# Converge a link: leave one already pointing at the target, and back up
# anything else in the way before linking. Get-Item -Force sees a link whose
# target is gone, which Test-Path reports as absent.
function Set-Link([string] $link, [string] $target, [string] $itemType) {
    $existing = Get-Item -LiteralPath $link -Force -ErrorAction SilentlyContinue
    if ($existing) {
        if ($existing.LinkTarget -eq $target) {
            return
        }
        Backup-Item $link
    }
    Write-Output "$link -> $target"
    New-Item -ItemType $itemType -Path $link -Target $target | Out-Null
}

# Converge a generated file to the given content, backing up anything else.
function Set-GeneratedFile([string] $path, [string] $content) {
    $existing = Get-Item -LiteralPath $path -Force -ErrorAction SilentlyContinue
    if ($existing) {
        # The interpolation turns what Get-Content -Raw returns for an empty
        # file into "", which .Trim() accepts; a [string] cast leaves it null.
        if (-not $existing.LinkType -and
            "$(Get-Content -LiteralPath $path -Raw)".Trim() -eq $content) {
            return
        }
        Backup-Item $path
    }
    Write-Output "$path = $content"
    Set-Content -Path $path -Value $content
}

# Dot files
Get-ChildItem -Path "$PSScriptRoot\.*"  | ForEach-Object {
    # ~/.claude holds the Claude CLI's credentials, transcripts and caches.
    # Linking it here would have the CLI write them inside this repository's
    # checkout.
    if (-not $_.Name.StartsWith(".git") -and $_.Name -ne ".claude") {
        $link = Join-Path -Path $env:HOME -ChildPath $_.Name
        $itemType = $_.PSIsContainer ? "Junction" : "SymbolicLink"
        Set-Link $link $_.FullName $itemType
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
    Set-Link "$terminalPath\settings.json" "$PSScriptRoot\etc\settings.json" "SymbolicLink"
}

# PowerShell profile
#
# Can't use symlink in Documents folder because OneDrive backup breaks. Emit a
# one line powershell profile that imports profile.ps1 from here instead.
$docs = [Environment]::GetFolderPath('Personal')
$psFolder = Join-Path -Path $docs -ChildPath "PowerShell"
New-Item -ItemType Directory -Path $psFolder -Force | Out-Null
$psProfile = Join-Path -Path $psFolder -ChildPath "Microsoft.PowerShell_profile.ps1"
Set-GeneratedFile $psProfile ". `"$PSScriptRoot\etc\profile.ps1`""
