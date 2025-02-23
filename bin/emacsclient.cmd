@if not defined _echo echo off
setlocal

if exist "%ProgramW6432%\Emacs\bin\runemacs.exe" (
  set emacsDir=%ProgramW6432%\Emacs\bin
  goto :Start
)

for /d %%d in ("%ProgramW6432%\Emacs\*") do (
  if exist "%%d\bin\runemacs.exe" (
    set emacsDir=%%d\bin
    goto :Start
  )
)

echo error: Emacs not found.
exit /b 1

:Start
set startArgs=
set clientArgs=%*
set createFrame=0
set fileArg=0

:ParseArgs
set arg=%1
if @%arg%@ == @@ (
  goto :Main
)

:: strip quotes
set arg=%arg:"=%
:: "<-- syntax highlighter thought there was quote to finish :)

if "%arg%" == "-c" (
   set createFrame=1
)
if "%arg%" == "--create-frame" (
  set createFrame=1
)
if not "%arg:~0,1%" == "-" (
  set fileArg=1
)
if not "%arg%" == "" (
  shift
  goto :ParseArgs
)

:Main
:: if not given a file and not told to create a frame, bring emacs into focus
if %fileArg% equ 0 (
    if %createFrame% equ 0 (
        set clientArgs=-e "(raise-frame)"
    )
)

:: if we're creating a frame, then start the initial instance minimized
if %createFrame% equ 1 (
  set startArgs=--iconic
)

:: see if server is already running
call :Ping
if %ERRORLEVEL% equ 0 goto :Ready

:: start a new emacs if not...
"%emacsDir%\runemacs.exe" %startArgs%

:: ...and wait until it responds
:Wait
%windir%\system32\timeout /t 1
call :Ping
if %ERRORLEVEL% equ 0 goto :Ready
goto :Wait

:Ready
"%emacsDir%\emacsclient.exe" %clientArgs%
exit /b %ERRORLEVEL%
goto :Eof

:Ping
"%emacsDir%\emacsclient.exe" -e nil >nul 2>nul
goto :Eof

:GetArg
set arg=%1
goto :Eof
