@echo off

:: set HOME=%USERPROFILE% permanently
:: emacs uses %APPDATA% otherwise
set HOME=%USERPROFILE%
setx HOME %HOME% >nul

setlocal enabledelayedexpansion

:: chain .gitconfig, allowing machine specific settings to be maintained using
:: `git config --global` while keeping only the settings that apply to all
:: machines in this repo
git config --global include.path %~dp0.gitconfig

:: make symbolic links to files
call :make_links

:: make junctions to directories
call :make_links /d /j

goto :eof

:make_links
for %1 %%f in (%~dp0.\.*) do (
    set filename=%%~nxf
    if "!filename:~0,4!" neq ".git" (
        if exist %HOME%\!filename! (
            echo warning: '%HOME%\!filename!' already exists, not overwriting
        ) else (
            mklink %2 %HOME%\!filename! %%f
        )
    )
)
goto :eof

