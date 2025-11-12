@setlocal
@prompt $G$S

winget install -e --id BurntSushi.ripgrep.MSVC
winget install -e --id Git.Git
winget install -e --id GitHub.cli
winget install -e --id GNU.Emacs
winget install -e --id icsharpcode.ILSpy
winget install -e --id KirillOsenkov.MSBuildStructuredLogViewer
winget install -e --id Microsoft.AzureCLI
winget install -e --id Microsoft.DotNet.SDK.8
winget install -e --id Microsoft.DotNet.SDK.9
winget install -e --id Microsoft.DotNet.SDK.10
winget install -e --id Microsoft.PowerShell
winget install -e --id Microsoft.VisualStudioCode
winget install -e --id Microsoft.WinDbg
winget install -e --id OpenJS.NodeJS.LTS
winget install -e --id ScooterSoftware.BeyondCompare.5
winget install -e --id TortoiseGit.TortoiseGit
winget install -e --id WinDirStat.WinDirStat

winget install -e --name "Sysinternals Suite" --source msstore
winget install -e --name "NuGet Package Explorer" --source msstore
