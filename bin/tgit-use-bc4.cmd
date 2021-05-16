:: Register Beyond Compare as TortoiseGit diff tool
@echo off
setlocal

set BC4Dir=%ProgramW6432%\Beyond Compare 4
if not exist "%BC4Dir%" (
  echo Beyond Compare 4 not found.
  exit /b 1
)

reg add HKCU\Software\TortoiseGit /v Diff  /t REG_SZ /d "\"%BC4Dir%\BComp.exe\" %%base %%mine /title1=%%bname /title2=%%yname /leftreadonly" /f > nul
reg add HKCU\Software\TortoiseGit /v Merge /t REG_SZ /d "\"%BC4Dir%\BComp.exe\" %%mine %%theirs %%base %%merged /title1=%%yname /title2=%%tname /title3=%%bname /title4=%%mname" /f > nul
