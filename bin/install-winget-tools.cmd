@setlocal
@prompt $G$S

winget install -e --id BurntSushi.ripgrep.MSVC
winget install -e --id Git.Git
winget install -e --id GitHub.cli
winget install -e --id KirillOsenkov.MSBuildStructuredLogViewer
winget install -e --id Microsoft.AzureCLI
winget install -e --id Microsoft.DotNet.SDK.8
winget install -e --id Microsoft.DotNet.SDK.9
winget install -e --id Microsoft.PowerShell
winget install -e --id Microsoft.VisualStudioCode
winget install -e --id Microsoft.WinDbg
winget install -e --id Microsoft.Sysinternals
winget install -e --id OpenJS.NodeJS.LTS
winget install -e --id ScooterSoftware.BeyondCompare.5
winget install -e --id TortoiseGit.TortoiseGit
winget install -e --id WinDirStat.WinDirStat

winget install -e --name "ILSpy Fresh" --source msstore --accept-package-agreements

:: Emacs installer doesn't register itself so running this script twice will
:: install twice without this check. To upgrade, change the version here.
@set EMACS_VER=30.1
@if exist "%ProgramW6432%\Emacs\emacs-%EMACS_VER%" goto :EmacsInstalled
winget install -e --id GNU.Emacs -v %EMACS_VER%
:EmacsInstalled