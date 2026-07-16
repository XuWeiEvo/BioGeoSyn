@echo off
setlocal
title BioGeoSyn

echo.
echo Starting BioGeoSyn...
echo.

set "RSCRIPT="

if defined R_SCRIPT (
  if exist "%R_SCRIPT%" set "RSCRIPT=%R_SCRIPT%"
)

if not defined RSCRIPT (
  if defined R_HOME (
    if exist "%R_HOME%\bin\Rscript.exe" set "RSCRIPT=%R_HOME%\bin\Rscript.exe"
  )
)

if not defined RSCRIPT (
  for /f "delims=" %%I in ('where Rscript.exe 2^>nul') do (
    if not defined RSCRIPT set "RSCRIPT=%%I"
  )
)

if not defined RSCRIPT call :FindRscript "%ProgramFiles%\R"
if not defined RSCRIPT call :FindRscript "%ProgramFiles(x86)%\R"
if not defined RSCRIPT call :FindRscript "%LOCALAPPDATA%\Programs\R"

if not defined RSCRIPT (
  echo ERROR: Rscript.exe was not found.
  echo.
  echo Install R first, or set the R_SCRIPT environment variable to the full path of Rscript.exe.
  echo Example:
  echo   C:\Program Files\R\R-4.3.1\bin\Rscript.exe
  echo.
  pause
  exit /b 1
)

echo Using Rscript:
echo   %RSCRIPT%
echo.
echo If a browser window does not open automatically, keep this window open and check the R output below.
echo.

set "IBGB_R_EXPR=options(shiny.launch.browser=TRUE); if (!requireNamespace('BioGeoSyn', quietly=TRUE)) stop('BioGeoSyn is not installed in this R library. Install it from GitHub first.', call.=FALSE); if (!requireNamespace('shiny', quietly=TRUE)) stop('The shiny package is not installed. Install shiny in R first.', call.=FALSE); BioGeoSyn::launch_app(launch.browser=TRUE)"

"%RSCRIPT%" -e "%IBGB_R_EXPR%"
set "EXITCODE=%ERRORLEVEL%"

echo.
if not "%EXITCODE%"=="0" (
  echo BioGeoSyn failed to start. Review the error message above.
) else (
  echo BioGeoSyn has stopped.
)
echo.
pause
exit /b %EXITCODE%

:FindRscript
if defined RSCRIPT exit /b 0
set "RROOT=%~1"
if "%RROOT%"=="" exit /b 0
if not exist "%RROOT%" exit /b 0
for /f "delims=" %%D in ('dir /b /ad /o-n "%RROOT%\R-*" 2^>nul') do (
  if exist "%RROOT%\%%D\bin\Rscript.exe" (
    set "RSCRIPT=%RROOT%\%%D\bin\Rscript.exe"
    exit /b 0
  )
)
exit /b 0
