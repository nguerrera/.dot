:: Register Beyond Compare as TortoiseGit diff tool
@echo off
setlocal

set BCDir=%ProgramW6432%\Beyond Compare 5
if not exist "%BCDir%" (
  echo Beyond Compare not found.
  exit /b 1
)

reg add HKCU\Software\TortoiseGit /v Diff  /t REG_SZ /d "\"%BCDir%\BComp.exe\" %%base %%mine /title1=%%bname /title2=%%yname /leftreadonly" /f > nul
reg add HKCU\Software\TortoiseGit /v Merge /t REG_SZ /d "\"%BCDir%\BComp.exe\" %%mine %%theirs %%base %%merged /title1=%%yname /title2=%%tname /title3=%%bname /title4=%%mname" /f > nul
