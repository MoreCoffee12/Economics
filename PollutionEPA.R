# Note that vignettes require knitr and rmarkdown
install.packages('knitr')
install.packages('rmarkdown')
install.packages('MazamaSpatialUtils')
devtools::install_github('MazamaScience/PWFSLSmoke', build_vignettes=TRUE, force=TRUE)


library(MazamaSpatialUtils)
dir.create('~/Data/Spatial', recursive=TRUE)
setSpatialDataDir('~/Data/Spatial')
installSpatialData()

library(MazamaScience/PWFSLSmoke)
zipFile <- epa_downloadData(2016, "88101", '~/Data/EPA')
tbl <- epa_parseData(zipFile, "PM2.5")

#install.packages("devtools")
#library(devtools)
#install_github("ebailey78/raqdm")
#library(raqdm)

