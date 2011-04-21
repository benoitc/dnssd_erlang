@echo off
setlocal

if "%BONJOUR_SDK_HOME%"=="" goto error_no_bsdk
if "%VCINSTALLDIR%"=="" goto call_vsvars

goto build

:error_no_bsdk
echo ERROR: BONJOUR_SDK_HOME variable is not set.
goto end

:call_vsvars
if "%VS90COMNTOOLS%"=="" goto error_no_vs
call "%VS90COMNTOOLS%\vsvars32.bat"
goto build

:build
escript.exe winbuild.escript
goto end

:error_no_vs
echo ERROR: VS90COMNTOOLS variable is not set.
goto end

:end
endlocal