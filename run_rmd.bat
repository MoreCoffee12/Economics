@echo off
setlocal

REM Directory of this batch file (with trailing \)
set "SCRIPT_DIR=%~dp0"
echo %SCRIPT_DIR%

REM Convert \ to /
set "SCRIPT_DIR_F=%SCRIPT_DIR:\=/%"

REM Document the final directories:
echo Using repo dir : %SCRIPT_DIR_F%
echo Output dir     : %OUT_DIR%

REM Output directory
set "OUT_DIR=C:/Users/Rainy/OneDrive/Documents/IssaquahDynamical/Datasets/2529RS0082_HistEconData"
echo %OUT_DIR%

REM Run Rmarkdown renderings relative to repo path
rscript -e "str_out <- '%OUT_DIR%'; rmarkdown::render('%SCRIPT_DIR_F%RecessionIndicators.Rmd', output_dir = str_out)"
rscript -e "str_out <- '%OUT_DIR%'; rmarkdown::render('%SCRIPT_DIR_F%GDPModels.Rmd', output_dir = str_out)"
rscript -e "str_out <- '%OUT_DIR%'; rmarkdown::render('%SCRIPT_DIR_F%Historical Relationships.Rmd', output_dir = str_out)"
rscript -e "str_out <- '%OUT_DIR%'; rmarkdown::render('%SCRIPT_DIR_F%UnemploymentModels.Rmd', output_dir = str_out)"

REM rscript -e "str_out <- 'C:/Users/Rainy/OneDrive/Documents/IssaquahDynamical/Datasets/2529RS0082_HistEconData'; rmarkdown::render('C:/Users/Rainy/Documents/GitHub/Economics/RecessionIndicators.Rmd', output_dir = str_out)"
REM rscript -e "str_out <- 'C:/Users/Rainy/OneDrive/Documents/IssaquahDynamical/Datasets/2529RS0082_HistEconData'; rmarkdown::render('D:/Local Documents/GitHub/Economics/RecessionIndicators.Rmd', output_dir = str_out)"
REM rscript -e "str_out <- 'C:/Users/Rainy/OneDrive/Documents/IssaquahDynamical/Datasets/2529RS0082_HistEconData'; rmarkdown::render('C:/Users/Rainy/Documents/GitHub/Economics/GDPModels.Rmd', output_dir = str_out)"
REM rscript -e "str_out <- 'C:/Users/Rainy/OneDrive/Documents/IssaquahDynamical/Datasets/2529RS0082_HistEconData'; rmarkdown::render('D:/Local Documents/GitHub/Economics/GDPModels.Rmd', output_dir = str_out)"
REM rscript -e "str_out <- 'C:/Users/Rainy/OneDrive/Documents/IssaquahDynamical/Datasets/2529RS0082_HistEconData'; rmarkdown::render('D:/Local Documents/GitHub/Economics/Historical Relationships.Rmd', output_dir = str_out)"
REM rscript -e "str_out <- 'C:/Users/Rainy/OneDrive/Documents/IssaquahDynamical/Datasets/2529RS0082_HistEconData'; rmarkdown::render('D:/Local Documents/GitHub/Economics/UnemploymentModels.Rmd', output_dir = str_out)"

pause