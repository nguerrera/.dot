@setlocal
@prompt $G$S

:: https://mastodon.social/@KirillOsenkov/111427666886735396
dotnet tool update -g binlogtool
dotnet tool update -g checkbinarycompat
dotnet tool update -g contentsync
dotnet tool update -g dotnet-ilrepack
dotnet tool update -g ff
dotnet tool update -g lbi
dotnet tool update -g lockchecktool
dotnet tool update -g pdb
dotnet tool update -g refdump
dotnet tool update -g sha
dotnet tool update -g timing

:: https://github.com/xoofx/ultra
dotnet tool update -g ultra

:: https://github.com/VerifyTests/Verify.Terminal
dotnet tool update -g verify.tool

:: https://learn.microsoft.com/en-us/dotnet/core/diagnostics/dotnet-trace
dotnet tool update -g dotnet-trace
