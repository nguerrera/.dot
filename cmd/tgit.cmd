:: Friendly wrapper around TortoiseGitProc.exe

@echo off
setlocal

if "%1" == "" (
    echo Usage: tgit ^<command^> [path]
    exit /b 1
)

::make sure this is a git repo
git rev-parse --show-toplevel > nul
if errorlevel 1 goto :eof

:: if the second argument is not a path, use the top of the 
:: repo as the path (just like git itself).
if "%2" == "" (
    for /f "delims=," %%p in ('git rev-parse --show-toplevel') do (
        set TGIT_PATH_ARG=%%p
        goto :tgit
    )
)

:: otherwise use the 2nd argument as the path.
set TGIT_PATH_ARG=%~dpnx2

:tgit
start "" "c:\Program Files\TortoiseGit\bin\TortoiseGitProc.exe" /command:%1 /path:"%TGIT_PATH_ARG%" %3 %4 %5 %6 %7 %8 %9
