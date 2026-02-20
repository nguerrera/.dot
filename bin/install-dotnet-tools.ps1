
# Tool links:
#  * https://mastodon.social/@KirillOsenkov/111427666886735396
#  * https://github.com/xoofx/ultra
#  * https://github.com/VerifyTests/Verify.Terminal
#  * https://learn.microsoft.com/en-us/dotnet/core/diagnostics/dotnet-trace

$tools = @(
    'binlogtool'
    'checkbinarycompat'
    'contentsync'
    'dotnet-ilrepack'
    'dotnet-trace'
    'ff'
    'lbi'
    'lockchecktool'
    'pdb'
    'refdump'
    'sha'
    'timing'
    'ultra'
    'verify.tool'
)

foreach ($tool in $tools) {
    dotnet tool update -g $tool
    if ($LASTEXITCODE -ne 0) { 
      exit $LASTEXITCODE
    }
}
