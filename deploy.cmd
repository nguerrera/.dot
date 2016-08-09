:: hard link to all of the dot files in this directory from the
:: home directory.

@echo off

setx HOME %HOME%

for %%f in (%~dp0.\.*) do (
    if exist %HOME%\%%~nxf (
        del /P %HOME%\%%~nxf
    )
    mklink /h %HOME%\%%~nxf %%f
)
