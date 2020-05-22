@echo off
setlocal
set EMACS_DIR=C:\Program Files\Emacs\bin

:: ping emacs server
call :ping
if %ERRORLEVEL% equ 0 goto :ready

:: start emacs if it didn't respond
"%EMACS_DIR%\runemacs.exe" -iconic

:: wait until it responds
:wait
%windir%\system32\timeout /t 1
call :ping
if %ERRORLEVEL% equ 0 goto :ready
goto :wait

:ping
"%EMACS_DIR%\emacsclient" -e nil >nul 2>nul
goto :eof

:ready
"%EMACS_DIR%\emacsclient" -c %*
