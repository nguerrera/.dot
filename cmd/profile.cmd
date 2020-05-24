@echo off

:: guard against appending to path more than once
@if not defined __PRIORPATH__ set __PRIORPATH__=%PATH%

:: clean up after any previous invocations
set PATH=%__PRIORPATH__%
set CommandPromptType=
set DevEnvDir=
set ExtensionSdkDir=
set Framework40Version=
set FrameworkDir=
set FrameworkDir32=
set FrameworkVersion=
set FrameworkVersion32=
set FSHARPINSTALLDIR=
set INCLUDE=
set LIB=
set LIBPATH=
set NETFXSDKDir=
set UCRTVersion=
set UniversalCRTSdkDir=
set VCIDEInstallDir=
set VCINSTALLDIR=
set VCToolsInstallDir=
set VCToolsRedistDir=
set VCToolsVersion=
set VisualStudioVersion=
set VS160COMNTOOLS=
set VSCMD_ARG_app_plat=
set VSCMD_ARG_HOST_ARCH=
set VSCMD_ARG_TGT_ARCH=
set VSCMD_VER=
set VSINSTALLDIR=
set WindowsLibPath=
set WindowsSdkBinPath=
set WindowsSdkDir=
set WindowsSDKLibVersion=
set WindowsSdkVerBinPath=
set WindowsSDKVersion=
set WindowsSDK_ExecutablePath_x64=
set WindowsSDK_ExecutablePath_x86=
set __DOTNET_ADD_32BIT=
set __DOTNET_PREFERRED_BITNESS=
set __VSCMD_PREINIT_PATH=
set __VSCMD_script_err_count=

:: include this directory in PATH
set "PATH=%~dp0;%PATH%"

:: add Beyond Compare to path if found
if exist "%ProgramW6432%\Beyond Compare 4" (
  set "PATH=%ProgramW6432%\Beyond Compare 4;%PATH%"
)

:: add hub to path if found, and alias git to itz
:: https://github.com/github/hub
if exist "%UserProfile%\OneDrive\Tools\Hub\bin\hub.exe" (
  set "PATH=%UserProfile%\OneDrive\Tools\Hub\bin;%PATH%"
  doskey git=hub $*
)

:: load macros
doskey /macrofile=%~dp0macros.ini

:: load VS developer command prompt
set VSCMD_START_DIR=%CD%
if exist "%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Preview\Common7\Tools\VSDevCmd.bat" (
  call "%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Preview\Common7\Tools\VSDevCmd.bat"
)

