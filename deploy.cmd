:: Symbolically link to all of the dot files in this directory from the home
:: directory. Chain in more git configuration layers.
@echo off

:: set HOME=%USERPROFILE% permanently
:: emacs uses %APPDATA% otherwise
set HOME=%USERPROFILE%
setx HOME %HOME% >nul
setlocal enabledelayedexpansion

git config --global include.path %~dp0etc\win.gitconfig

:: Disable safe directories so Windows git can operate on WSL mounts. For
:: some reason, this has to be in the top-level config file. And it also
:: does not support wildcard other than single '*' to match absolutely
:: everything. :(
git config --global safe.directory *

call :make_links
call :make_links /d /j

if exist %LOCALAPPDATA%\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState (
  call :make_link %LOCALAPPDATA%\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json %~dp0etc\settings.json
)

if exist %LOCALAPPDATA%\Packages\Microsoft.WindowsTerminalPreview_8wekyb3d8bbwe\LocalState (
  call :make_link %LOCALAPPDATA%\Packages\Microsoft.WindowsTerminalPreview_8wekyb3d8bbwe\LocalState\settings.json %~dp0etc\settings.json
)

:: Can't use symlink in Documents folder because OneDrive backup breaks
:: Emit a one line powershell profile that imports profile.ps1 from here instead
for /f "usebackq tokens=*" %%a in (`powershell -command "[Environment]::GetFolderPath('Personal')"`) do (set DOCS=%%a)
mkdir "%DOCS%\PowerShell"
set PS_PROFILE=%DOCS%\PowerShell\Microsoft.PowerShell_profile.ps1
if exist "%PS_PROFILE%" (
  echo warning: '%PS_PROFILE%' already exists, not overwriting
) else (
  echo y
  echo Creating %PS_PROFILE%
  echo . %~dp0etc\profile.ps1 > "%PS_PROFILE%"
)
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

