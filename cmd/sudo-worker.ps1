# Provides the powershell implementation details for sudo.cmd
# Which elevates to run the given command if necessary.

param([switch] $NoExit, [switch] $NoProfile, [switch] $Trace)

$ErrorActionPreference = "Stop"
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
$currentDir = Get-Location
$command = "cd /d $currentDir"

if (!$NoProfile)
{
    $command = "$command && $scriptDir\profile.cmd"
}

if ($args)
{
    $args = $args | % { if ($_.Contains(" ")) { "`"$_`"" } else { $_ } }
    $command += "&& " + [String]::Join(" ", $args)
}
else
{
    $NoExit = $true
}

if ($NoExit)
{
    $command = "/k $command";
}
else
{
    $command = "/c $command";
}

if ($Trace)
{
    Write-Host "> cmd.exe $command"
}

Start-Process -Verb RunAs -FilePath cmd.exe -ArgumentList $command -WorkingDirectory (Get-Location)