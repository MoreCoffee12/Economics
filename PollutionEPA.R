# Note that vignettes require knitr and rmarkdown
#install.packages('knitr')
#install.packages('rmarkdown')
#install.packages('MazamaSpatialUtils')
#devtools::install_github('MazamaScience/PWFSLSmoke', build_vignettes=TRUE, force=TRUE)


#library(MazamaSpatialUtils)
#dir.create('~/Data/Spatial', recursive=TRUE)
#setSpatialDataDir('~/Data/Spatial')
#installSpatialData()

library(PWFSLSmoke)
library(xts)
library(ggplot2)

# Download 88101 - PM2.5. These are saved to local store so only need to download
# them once
zipFile <- epa_downloadData(2020, "88101", '~/Data/EPA')

# Load data from local repository
strFilePre <- "C:/Users/Rainy/Documents/Data/EPA/hourly_88101_" 
for (iYear in 2010:2019) {
  strFile <- paste(strFilePre, iYear, ".zip", sep="")
  print(strFile)

  # Load the data
  tbl <- epa_parseData(strFile)

  # Subset out the King county data
  tblSeattle = tbl[(tbl$'State Code'==53 & tbl$`Site Num`==2004),]

  # Concatenate the date and time to convert to data object
  # tblSeattle$dtDateTimeLocal <- as.Date.character(paste(tblSeattle$`Date Local`, tblSeattle$`Time Local`), "%Y-%m-%d %H:%M")
  # tblSeattle$dtDateTimeLocal <- as.POSIXct(strptime(paste(tblSeattle$`Date Local`, tblSeattle$`Time Local`), "%Y-%m-%d %H:%M"))
  tblSeattle$dtDateTimeLocal <- as.POSIXct(strptime(tblSeattle$`Date Local`, "%Y-%m-%d"))
  
  # Uncomment this line to check the conversion
  strftime(tblSeattle$dtDateTimeLocal[4], format='%d/%b/%Y:%H:%M:%S')
 
  # Create daily average table
  tblSeattleDaily <- aggregate(tblSeattle$`Sample Measurement`, by=list(tblSeattle$dtDateTimeLocal), mean)
  
  if ( iYear == 2010){
    xtsPCMDaily <- as.xts(tblSeattleDaily$x, tblSeattleDaily$Group.1)
  }else{
    xtsPCMDaily <- rbind(xtsPCMDaily, as.xts(tblSeattleDaily$x, tblSeattleDaily$Group.1))
    
  }
}

xts.org <- xtsPCMDaily

xtsPCMDaily <- xts.org

# naming, adding ytd day count
names(xtsPCMDaily) <- "Sample"
xtsPCMDaily$ytd.day = as.numeric(strftime(index(xtsPCMDaily), "%j"))
xtsPCMDaily$cal.year = as.factor(strftime(index(xtsPCMDaily), "%Y"))
xtsPCMDaily$month.number = format(index(xtsPCMDaily), "%m")

# aggregate days across the years
df.daily <- data.frame(Sample = xtsPCMDaily$Sample, ytd.day = xtsPCMDaily$ytd.day )
df.daily <- aggregate(df.daily$Sample, by=list(df.daily$ytd.day), mean)
colnames(df.daily) <- c("ytd.day", "Sample")
df.daily$cal.year = 2000

# Plot it out
p1 <- ggplot() +
  geom_point(data=xtsPCMDaily, aes(x=ytd.day, y=Sample, color=cal.year )) 
p1

p1 <- ggplot(NULL, aes(ytd.day, Sample, colour=cal.year))+
  geom_point(data=xtsPCMDaily) +
  geom_line(data=df.daily)  
p1






