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

