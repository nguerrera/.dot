:: Symbolically link to all of the dot files in this directory from the
:: home directory.

@echo off

net file >nul 2> nul && goto :Elevated
echo error: Administrator rights are required to make symbolic links on Windows.
exit /b 1


:Elevated
if not defined HOME (
    set HOME=%USERPROFILE%
)

for %%f in (%~dp0.\.*) do (
    if exist %HOME%\%%~nxf (
        del /P %HOME%\%%~nxf
    )
    mklink %HOME%\%%~nxf %%f
)
