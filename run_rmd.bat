@echo off
setlocal

REM Directory of this batch file (with trailing \)
set "SCRIPT_DIR=%~dp0"

REM Convert \ to /
set "SCRIPT_DIR_F=%SCRIPT_DIR:\=/%"

REM Output directory
set "OUT_DIR=C:/Users/Rainy/OneDrive/Documents/IssaquahDynamical/Datasets/2529RS0082_HistEconData"

REM Use the updated package overlay before the standard user library.
set "R_LIBS_USER=%LOCALAPPDATA%/R/win-library/4.5-overlay;%LOCALAPPDATA%/R/win-library/4.5"
set "RSCRIPT=C:\Program Files\R\R-4.5.2\bin\Rscript.exe"

echo Using repo dir : %SCRIPT_DIR_F%
echo Output dir     : %OUT_DIR%
echo R library path : %R_LIBS_USER%

REM Run Rmarkdown renderings relative to repo path
call :render "RecessionIndicators.Rmd" || exit /b 1
call :render "GDPModels.Rmd" || exit /b 1
call :render "UnemploymentModels.Rmd" || exit /b 1
call :render "Historical Relationships.Rmd" || exit /b 1
call :render "RecessionModels.Rmd" || exit /b 1
exit /b 0

:render
echo Rendering %~1
"%RSCRIPT%" -e "str_out <- '%OUT_DIR%'; rmarkdown::render('%SCRIPT_DIR_F%%~1', output_dir = str_out)"
exit /b %ERRORLEVEL%

