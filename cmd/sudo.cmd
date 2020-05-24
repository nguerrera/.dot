:: Elevates (if necessary) and runs the given command
:: If no command is given, starts a new admin window.
@echo off
setlocal enabledelayedexpansion

net file >nul 2> nul && goto :Elevated

:: Convert quotes to single quotes because the args aren't getting
:: split correctly by powershell otherwise.
set _args= %*
if not "!_args! " == " " set _args=!_args:"='!

powershell -ExecutionPolicy RemoteSigned %~dp0sudo-worker.ps1 %_args%
goto :eof

:Elevated
set _args=%*
if " %1 " == " -NoProfile " set _args=start /i %_args:~11%
%_args%
goto :eof