:: hard link to all of the dot files in this directory from the
:: home directory.

@echo off

if not defined HOME (
    set HOME=%USERPROFILE%
)

setx HOME %HOME%

for %%f in (%~dp0.\.*) do (
    if  "%%~nxf" neq ".gitconfig" (
        if exist %HOME%\%%~nxf (
            del /P %HOME%\%%~nxf
        )
        mklink %HOME%\%%~nxf %%f
    )
)

git config --global include.path %~dp0.gitconfig
