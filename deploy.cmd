:: Symbolically link to all of the dot files in this directory from the home
:: directory. Chain in more git configuration layers.
@echo off

:: set HOME=%USERPROFILE% permanently
:: emacs uses %APPDATA% otherwise
set HOME=%USERPROFILE%
setx HOME %HOME% >nul
setlocal enabledelayedexpansion

git config --global include.path %~dp0git\win.gitconfig

call :make_links
call :make_links /d /j
call :make_link %LOCALAPPDATA%\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json %~dp0cmd\settings.json
call :make_link %USERPROFILE%\Documents\PowerShell\Microsoft.PowerShell_profile.ps1 %~dp0cmd\profile.ps1
goto :eof

:make_links
for %1 %%f in (%~dp0.\.*) do (
    set filename=%%~nxf
    if "!filename:~0,4!" neq ".git" (
        call :make_link %HOME%\!filename! %%f %2
    )
)
goto :eof

:make_link
if exist %1 (
    echo warning: '%1' already exists, not overwriting
) else (
    mklink %3 %1 %2
)

goto :eof

