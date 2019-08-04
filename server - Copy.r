library(shiny)
library(UsingR)
library(quantmod)
library(ggplot2)
library(gridExtra)

# setwd('D:/Documents/SignalProcessing/Economics')

# set the R_WIN_INTERNET2 to TRUE
#setInternet2(TRUE) 

getSymbols("CPIAUCSL",src="FRED")
getSymbols("USREC",src="FRED")
getSymbols("UNRATE", src="FRED")
getSymbols("INDPRO", src="FRED")
getSymbols("RRSFS", src="FRED")
getSymbols("RSALES", src="FRED")
getSymbols("W875RX1A020NBEA", src="FRED")
getSymbols("PCOPPUSDM",src="FRED")
getSymbols("NOBL", src="yahoo")

getSymbols("BUSLOANS", src="FRED")

# Nonfinancial corporate business (FRED)
getSymbols("NCBDBIQ027S", src="FRED")

getSymbols("REALLNNSA", src="FRED")

getSymbols("CONSUMERNSA", src="FRED")

getSymbols("DGS10", src="FRED")

getSymbols("DGS1", src="FRED")

getSymbols("DCOILWTICO", src="FRED")

getSymbols("NEWORDER", src="FRED")

getSymbols("DCPF3M", src="FRED")

getSymbols("DCPF1M", src="FRED")

# Cast the xts time series to a data frame
cpi.df <- data.frame(date = index(CPIAUCSL), CPIAUCSL$CPIAUCSL)
saveRDS(cpi.df, file="CPIAUCSL.rds")
#cpi.df <- readRDS("CPIAUCSL.rds")

usrec.df <- data.frame(date = index(USREC), USREC$USREC)
saveRDS(usrec.df, file="USREC.rds")
#usrec.df <- readRDS("USREC.rds")

unrate.df <- data.frame(date= index(UNRATE),UNRATE$UNRATE)
saveRDS(unrate.df, file="UNRATE.rds")
#unrate.df <- readRDS("UNRATE.rds")

indpro.df <- data.frame(date= index(INDPRO),INDPRO$INDPRO)
saveRDS(indpro.df, file="INDPRO.rds")
#indpro.df <- readRDS("INDPRO.rds")

rrsfs.df <- data.frame(date= index(RRSFS), RRSFS$RRSFS)
saveRDS(rrsfs.df, file="RRSFS.rds")
#rrsfs.df <- readRDS("RRSFS.rds")

rsales.df <- data.frame(date = index(RSALES), RSALES$RSALES)
saveRDS(rsales.df, file="RSALES.rds")
#rsales.df <- readRDS("RSALES.rds")

rpietr.df <- data.frame(date= index(W875RX1A020NBEA), W875RX1A020NBEA$W875RX1A020NBEA)
saveRDS(rpietr.df, file="W875RX1A020NBEA.rds")
#rpietr.df <- readRDS("W875RX1A020NBEA.rds")

copper.df <- data.frame(date = index(PCOPPUSDM), PCOPPUSDM$PCOPPUSDM)
saveRDS(copper.df, file = "PCOPPUSDM.rds")
#copper.df <- readRDS("PCOPPUSDM.rds")

busloans.df <- data.frame(date = index(BUSLOANS), BUSLOANS$BUSLOANS)
saveRDS(busloans.df, file = "BUSLOANS.rds")
Nrow <- nrow(busloans.df)
busloans.df$GrowthRateYoY <- rep(0,Nrow)
busloans.df$GrowthRateYoY[13:Nrow] <- diff(as.matrix(busloans.df$BUSLOANS), lag = 12)
busloans.df$GrowthRateYoY <- (busloans.df$GrowthRateYoY / busloans.df$BUSLOANS)*100

# Nonfinancial corporate business (FRED)
NCBDBIQ027S.df <- data.frame(date = index(NCBDBIQ027S), NCBDBIQ027S$NCBDBIQ027S)
saveRDS(NCBDBIQ027S.df, file = "NCBDBIQ027S.rds")
Nrow <- nrow(NCBDBIQ027S.df)
NCBDBIQ027S.df$GrowthRateYoY <- rep(0,Nrow)
NCBDBIQ027S.df$GrowthRateYoY[13:Nrow] <- diff(as.matrix(NCBDBIQ027S.df$NCBDBIQ027S), lag = 12)
NCBDBIQ027S.df$GrowthRateYoY <- (NCBDBIQ027S.df$GrowthRateYoY / NCBDBIQ027S.df$NCBDBIQ027S)*100

realestateloans.df <- data.frame(date = index(REALLNNSA), REALLNNSA$REALLNNSA)
saveRDS(realestateloans.df, file = "REALLNNSA.rds")

# Consumer loans
consloans.df <- data.frame(date = index(CONSUMERNSA), CONSUMERNSA$CONSUMERNSA)
saveRDS(consloans.df, file = "CONSUMERNSA.rds")

# Total loans
totloans.df <- data.frame(date = busloans.df$date, tot = busloans.df$BUSLOANS+
                              realestateloans.df$REALLNNSA+
                              consloans.df$CONSUMERNSA)
Nrow <- nrow(totloans.df)
totloans.df$GrowthRateYoY <- rep(0,Nrow)
totloans.df$GrowthRateYoY[13:Nrow] <- diff(as.matrix(totloans.df$tot), lag = 12)
totloans.df$GrowthRateYoY <- (totloans.df$GrowthRateYoY / totloans.df$tot)*100

# 10 year yield curve
bond10.df <- data.frame(date = index(DGS10), DGS10$DGS10)
saveRDS(bond10.df, file = "DGS10.rds")

# 1 year yield curve
bond1.df <- data.frame(date = index(DGS1), DGS1$DGS1)
saveRDS(bond1.df, file = "DGS1.rds")

# Calculate the 10 year vs 1 year yield curve
bondcv.df <- bond1.df
bondcv.df$TenOne <- rep(0,nrow(bondcv.df))
bondcv.df$TenOne <- as.matrix(bond10.df$DGS10) - as.matrix(bond1.df$DGS1)

# Crude, WTI
DCOILWTICO.df <- data.frame(date = index(DCOILWTICO), DCOILWTICO$DCOILWTICO)
saveRDS(DCOILWTICO.df, file = "DCOILWTICO.rds")

#Read in the 3500 sales counts
count3500.df = csv.get('350040_350092.csv')
count3500.df$Period = as.character(count3500.df$Period)
posDot <- regexpr('[.]',count3500.df$Period)
count3500.df$Year <- substr(count3500.df$Period,posDot+1, nchar(count3500.df$Period))
count3500.df$Period <- as.numeric(substr(count3500.df$Period, 1, posDot-1))
count3500.df$Days <- count3500.df$Period*4*7
count3500.df$YearPOSIX <- as.POSIXct(strptime(paste(count3500.df$Year, "-01-01 00:00:00", sep = ""), "%Y-%m-%d %H:%M:%S"))
count3500.df$date <- as.Date(as.POSIXct(count3500.df$YearPOSIX+(3600*24*count3500.df$Days),origin = "1970-01-01"))
count3500.df$EndUser.Quantity <- as.character(count3500.df$EndUser.Quantity)
count3500.df$EndUser.Quantity <- as.numeric(gsub(",","",count3500.df$EndUser.Quantity))
count3500.df$EndUser.Bookings <- as.character(count3500.df$EndUser.Bookings)
count3500.df$EndUser.Bookings <- as.numeric(gsub(",","",count3500.df$EndUser.Bookings))
count3500.df$EndUser.Bookings.MM <- count3500.df$EndUser.Bookings/1000000;
count3500.df$X <- NULL
count3500.df$EndUsr.Itm <- NULL
count3500.df$Indirect1.Bookings <- NULL
count3500.df$X.1 <- NULL
count3500.df <- na.omit(count3500.df)

# Manufacturers new orders, also create trailing 12 month index
NEWORDER.df <- data.frame(date = index(NEWORDER), NEWORDER$NEWORDER)
saveRDS(NEWORDER.df, file = "NEWORDER.rds")
Nrow <- nrow(NEWORDER.df)
NEWORDER.df$GrowthRateYoY <- rep(0,Nrow)
NEWORDER.df$GrowthRateYoY[13:Nrow] <- diff(as.matrix(NEWORDER.df$NEWORDER), lag = 12)
NEWORDER.df$GrowthRateYoY <- (NEWORDER.df$GrowthRateYoY / NEWORDER.df$NEWORDER)*100

# 3-Month AA Financial Commercial Paper Rate
DCPF3M.df <- data.frame(date = index(DCPF3M), DCPF3M$DCPF3M)
saveRDS(DCPF3M.df, file = "DCPF3M.rds")

# 1-Month AA Financial Commercial Paper Rate
DCPF1M.df <- data.frame(date = index(DCPF1M), DCPF1M$DCPF1M)
saveRDS(DCPF1M.df, file = "DCPF1M.rds")

# Calculate the 3-month vs 1-month AA commercial curve
if (nrow(DCPF1M.df) != nrow(DCPF3M.df)){

  iMinRow = min(c(nrow(DCPF1M.df),nrow(DCPF3M.df)))
  DCPF1M.df <- DCPF1M.df[1:iMinRow,]
  DCPF3M.df <- DCPF3M.df[1:iMinRow,]

}
AAcomcv.df <- DCPF3M.df
AAcomcv.df$ThreeOne <- rep(0,nrow(AAcomcv.df))
AAcomcv.df$ThreeOne <- as.matrix(DCPF3M.df$DCPF3M) - as.matrix(DCPF1M.df$DCPF1M)


# The Fred recession data is 1 for the months that
# are in recession and 0 for months not in recession
# so we will use the diff command to save off the 
# indexes where the value changes from 0 to 1 or
# 1 to 0. I found this idea in a stack overflow
# article:
# http://stackoverflow.com/questions/21739012/r-recession-dates-conversion
# I found it was more robust than the nberShade()
# command for the xts time series data.
#start <- index(USREC[which(diff(USREC$USREC)==1)])
start <- usrec.df$date[which(diff(usrec.df$USREC)==1)+1]
#end   <- index(USREC[which(diff(USREC$USREC)==-1)-1])
end   <- usrec.df$date[which(diff(usrec.df$USREC)==-1)]

# We need to cast the recession data into
# a dataframe.
reccesion.df <- data.frame(start=start, end=end[-1])
recession.df <- subset(reccesion.df, start >= min(unrate.df$date))

# This function creates the unemployment plot
plotCPI <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=cpi.df, aes(x=date,y=CPIAUCSL)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) + 
    ggtitle("Consumer Price Index for All Urban Consumers: All Items (CPIAUCSL)") + 
    labs(x="Date", y = "Index 1892-1894=100") +
    scale_y_continuous(limits = c(0, 250)) +    
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()) )
}

# This function creates the unemployment plot
plotUnemp <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=unrate.df, aes(x=date,y=UNRATE)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) + 
    ggtitle("US U-3 Unemployment rate (UNRATE)") + 
    labs(x="Date", y = "Percent Unemployed") +
    scale_y_continuous(limits = c(0, 15)) +    
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()) )
}

# Industrial production
plotProd <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=indpro.df, aes(x=date,y=INDPRO)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Industrial production (INDPRO)") + 
    labs(x="Date", y = "Index 2012 = 100") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
}

# Real retail and food service sales
plotRetail <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=rrsfs.df, aes(x=date, y=RRSFS, colour = "RRSFS")) +
    geom_line(data=rsales.df, aes(x=date, y=RSALES, colour = "RSALES")) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Real Retail and Food Services Sales (RRSFS and RSALES)") + 
    labs(x="Date", y = "Millions of Dollars") + 
    theme(legend.title=element_blank()) + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date())) 
  
}

# Real personal income
plotPerIncome <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=rpietr.df, aes(x=date,y=W875RX1A020NBEA)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Real personal income excluding current transfer receipts (W875RX1A020NBEA)") + 
    labs(x="Date", y = "Billions of Chained 2009 dollars") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
  
}

# Doctor copper 
plotCopper <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=copper.df, aes(x=date,y=PCOPPUSDM)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("UnitedGlobal price of Copper (PCOPPUSDM)") + 
    labs(x="Date", y = "USD") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
  
}

# Plot the business loans in the US
plotBusLoans <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=busloans.df, aes(x=date,y=BUSLOANS)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Commercial and Industrial Loans, All Commercial Banks (BUSLOANS)") + 
    labs(x="Date", y = "Billions of US dollars") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
}

# Plot the year over year (YoY) business loans in the US
plotBusLoansYoY <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=busloans.df, aes(x=date,y=GrowthRateYoY)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Year over Year Growth of Commercial and Industrial Loans, All Commercial Banks (BUSLOANS)") + 
    labs(x="Date", y = "Percent") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
}

# Plot the Nonfinancial corporate business loans in the US
plotNonFinCorpLoans <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=NCBDBIQ027S.df, aes(x=date,y=NCBDBIQ027S/1000000)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Nonfinancial corporate business (FRED: NCBDBIQ027S)") + 
    labs(x="Date", y = "Millions of US dollars") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
}

# Plot the Nonfinancial corporate business loans YoY in the US
plotNonFinCorpLoansYoY <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    #theme(panel.grid.minor = element_line(colour="black", size=0.5)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(0 , 30, 5), breaks = seq(0, 30, 10)) +
    geom_line(data=NCBDBIQ027S.df, aes(x=date,y=GrowthRateYoY, colour = "YoYLoan") ) +
    geom_line(data=bondcv.df, aes(x=date,y=TenOne, colour = "TenMinusOne")) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("YoY Growth Nonfinancial corporate business loans (FRED: NCBDBIQ027S)") + 
    labs(x="Date", y = "Percent") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date())) + 
    scale_colour_manual("",
                        breaks = c("YoYLoan", "TenMinusOne"),
                        values = c("BLACK", "RED"),
                        labels = c("YoY Corp Business Loan Growth", "10-Year minus 1-Year"))
}



# Plot the real estate loans in the US
plotRealEstateLoans <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=realestateloans.df, aes(x=date,y=REALLNNSA)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Real Estate Loans, All Commercial Banks (REALLNNSA)") + 
    labs(x="Date", y = "Billions of US dollars") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
}

# Plot the consumer loans in the US
plotConsLoans <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=consloans.df, aes(x=date,y=CONSUMERNSA)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Consumer Loans, All Commercial Banks (CONSUMERNSA)") + 
    labs(x="Date", y = "Billions of US dollars") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
}

# Plot the total loans in the US
plotTotLoans <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=totloans.df, aes(x=date,y=tot)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("C&I, Real Estate and Consumer Loans, All Commercial Banks") + 
    labs(x="Date", y = "Billions of US dollars") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
}

# Plot the total loans YoY in the US
plotTotLoansYoY <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    #theme(panel.grid.minor = element_line(colour="black", size=0.5)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(0 , 30, 5), breaks = seq(0, 30, 10)) +
    geom_line(data=totloans.df, aes(x=date,y=GrowthRateYoY, colour = "YoYLoan") ) +
    geom_line(data=bondcv.df, aes(x=date,y=TenOne, colour = "TenMinusOne")) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("YoY Growth C&I, Real Estate and Consumer Loans, All Commercial Banks") + 
    labs(x="Date", y = "Percent") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date())) + 
    scale_colour_manual("",
                        breaks = c("YoYLoan", "TenMinusOne"),
                        values = c("BLACK", "RED"),
                        labels = c("YoY Total Loan Growth", "10-Year minus 1-Year"))
}

# Plot the ten year yield
plotBond10 <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=bond10.df, aes(x=date,y=DGS10)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("10-Year Treasury Constant Maturity Rate (DGS10)") + 
    labs(x="Date", y = "Percent") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
}

# Plot the one year yield
plotBond1 <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    geom_line(data=bond1.df, aes(x=date,y=DGS1)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("1-Year Treasury Constant Maturity Rate (DGS1)") + 
    labs(x="Date", y = "Percent") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
}

# Plot the ten year minus the one year
plotBondCV <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    #theme(panel.grid.minor = element_line(colour="black", size=0.5)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(0 , 30, 5), breaks = seq(0, 30, 10)) +
    geom_line(data=bondcv.df, aes(x=date,y=TenOne)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("10-Year minus 1-Year Treasury Constant Maturity Rate") + 
    labs(x="Date", y = "Percent") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
}

# Plot crude oil prices
plotCrude <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    #theme(panel.grid.minor = element_line(colour="black", size=0.5)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(0 , 140, 5), breaks = seq(0, 140, 10)) +
    geom_line(data=DCOILWTICO.df, aes(x=date,y=DCOILWTICO)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Crude, WTI (FRED: DCOILWTICO)") + 
    labs(x="Date", y = "Dollars per Barrel") + 
    scale_x_date(limits = c(as.Date("1jan1945","%d%b%Y"), Sys.Date()))
}

# Plot crude oil prices and Bently
plotCrudeBently <- function() {
  
  myplot1 <- ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    #theme(panel.grid.minor = element_line(colour="black", size=0.5)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(0 , 14000, 500), breaks = seq(0, 14000, 1000)) +
    geom_line(data=count3500.df, aes(x=date,y=EndUser.Quantity)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Number of 3500/XX Monitors Shipped") + 
    labs(x="Date", y = "Monitors Shipped") + 
    scale_x_date(limits = c(as.Date("1jan2004","%d%b%Y"), Sys.Date()))

  myplot2 <- ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    #theme(panel.grid.minor = element_line(colour="black", size=0.5)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(0 , 140, 5), breaks = seq(0, 140, 10)) +
    geom_line(data=DCOILWTICO.df, aes(x=date,y=DCOILWTICO)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Crude, WTI (FRED: DCOILWTICO)") + 
    labs(x="Date", y = "Dollars per Barrel") + 
    scale_x_date(limits = c(as.Date("1jan2004","%d%b%Y"), Sys.Date()))
  
  grid.arrange(myplot1, myplot2)

}

# Plot New orders
plotNewOrders <- function() {
  
  myplot1 <- ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    #theme(panel.grid.minor = element_line(colour="black", size=0.5)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(0 , 75000, 3750), breaks = seq(0, 75000, 7500)) +
    geom_line(data=NEWORDER.df, aes(x=date,y=NEWORDER)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Manufacturers' New Orders: Nondefense Capital Goods Excluding Aircraft (NEWORDER)") + 
    labs(x="Date", y = "Millions of Dollars") + 
    scale_x_date(limits = c(as.Date("1jan1992","%d%b%Y"), Sys.Date()))
  
  myplot2 <- ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    #theme(panel.grid.minor = element_line(colour="black", size=0.5)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(-60, 20, 5), breaks = seq(-60, 20, 20)) +
    geom_line(data=NEWORDER.df, aes(x=date,y=GrowthRateYoY)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("Manufacturers' New Orders: Nondefense Capital Goods Excluding Aircraft (NEWORDER) YoY") + 
    labs(x="Date", y = "% YoY Increase") + 
    scale_x_date(limits = c(as.Date("1jan1992","%d%b%Y"), Sys.Date()))
  
  grid.arrange(myplot1, myplot2)
  
}

# Plot AA comercial paper
plotAACom <- function() {
  
  myplot1 <- ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    #theme(panel.grid.minor = element_line(colour="black", size=0.5)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(0, 10, 1), breaks = seq(0, 10, 1)) +
    geom_line(data=DCPF3M.df, aes(x=date,y=DCPF3M)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("3-Month AA Financial Commercial Paper Rate (DCPF3M)") + 
    labs(x="Date", y = "Percent") + 
    scale_x_date(limits = c(as.Date("1jan1992","%d%b%Y"), Sys.Date()))
  
  myplot2 <- ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    #theme(panel.grid.minor = element_line(colour="black", size=0.5)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(0, 10, 1), breaks = seq(0, 10, 1)) +
    geom_line(data=DCPF1M.df, aes(x=date,y=DCPF1M)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("1-Month AA Financial Commercial Paper Rate (DCPF1M)") + 
    labs(x="Date", y = "Percent") + 
    scale_x_date(limits = c(as.Date("1jan1992","%d%b%Y"), Sys.Date()))
  
  grid.arrange(myplot1, myplot2)
  
}


# Plot the 3 month minus the one month AA
plotAAComCV <- function() {
  ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    #theme(panel.grid.minor = element_line(colour="black", size=0.5)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(0 , 3, 0.5), breaks = seq(0, 3, 0.5)) +
    geom_line(data=AAcomcv.df, aes(x=date,y=ThreeOne)) +
    geom_rect(data=recession.df,
              aes(xmin=start,xmax=end, ymin=-Inf, ymax=Inf), 
              fill="grey", alpha=0.3) +
    ggtitle("3-Month (DCPF3M) minus 1-Month (DCPF1M) AA Commercial Paper") + 
    labs(x="Date", y = "Percent") + 
    scale_x_date(limits = c(as.Date("1jan1997","%d%b%Y"), Sys.Date()))
}


# This function selects the appropriate chart
plotType <- function(type) {

    switch(type,
         plotCPI(),
         plotUnemp(),
         plotProd(),
         plotRetail(),
         plotPerIncome(),
         plotCopper(),
         plotBusLoans(),
         plotBusLoansYoY(),
         plotNonFinCorpLoans(),
         plotNonFinCorpLoansYoY(),
         plotRealEstateLoans(),
         plotConsLoans(),
         plotTotLoans(),
         plotTotLoansYoY(),
         plotBond10(),
         plotBond1(),
         plotBondCV(),
         plotCrude(),
         plotCrudeBently(),
         plotNewOrders(),
         plotAACom(),
         plotAAComCV())
}

# This function updates the text
updateText <- function(type) {
  
  switch(type,
         "The Consumer Price Index for All Urban Consumers: All Items (CPIAUCSL) is a measure of the average monthly change in the price for goods and services paid by urban consumers between any two time periods.(1) It can also represent the buying habits of urban consumers. This particular index includes roughly 88 percent of the total population, accounting for wage earners, clerical workers, technical workers, self-employed, short-term workers, unemployed, retirees, and those not in the labor force.(1) ",
         "This is the U-3 unemployment rate generated by a survey of housholds. Unemployement tends to flatten just before and then rise during a recession. Shaded areas indicate NBER recessions.",
         "FRED industrial production index provides an indication of output at manufacturing, mining, electric utilities, and gas utilities. This usually falls during a recession. Shaded areas indicate NBER recessions.",
         "The RSALES data indicates the consumer price index based on the standard Consumer Industrial Classification (CIS) system and was maintained until 1 April 2001. Starting on 1 Jan 1992 the RRSFS data was calculated using the North American Industry Classification System (NAICS). Both datasets are deflated using the Consumer Price Index (CPI). Retail sales tend to flatten before and fall during a recession. Shaded areas indicate NBER recessions.",
         "This is real (inflation adjusted) personal income. The series originates with the Bureau of Economic Analysis (BEA), but is reported through FRED. This tends to flatten just before a recession and then falls. Shaded areas indicate NBER recessions.",
         "This is Yahoo's copper price series.",
         "This is Commercial and Industrial Loans, All Commercial Banks, Billions of U.S. Dollars, Seasonally Adjusted from FRED (BUSLOANS).",
         "This is YoY growth of Commercial and Industrial Loans, All Commercial Banks, Billions of U.S. Dollars, Seasonally Adjusted from FRED (BUSLOANS).",
         "Nonfinancial corporate business; debt securities; liability, Level from FRED (NCBDBIQ027S).",
         "Nonfinancial corporate business; debt securities; liability, Year over year growth. From FRED (NCBDBIQ027S).",
         "This is Real Estate Loans, All Commercial Banks, Billions of U.S. Dollars, Not Seasonally Adjusted from FRED (REALLNNSA).",
         "This is C&I, Real Estate, and Consumer Loans, Billions of U.S. Dollars, Not Seasonally Adjusted from FRED.",
         "This is YoY Growth C&I, Real Estate, and Consumer Loans, Billions of U.S. Dollars, Not Seasonally Adjusted from FRED.",
         "This is 10-Year Treasury Constant Maturity Rate, Daily, Not Seasonally Adjusted (DGS10) from FRED.",
         "West Texas Intermediate (WTI) - Cushing, Oklahoma (DCOILWTICO) from FRED.",
         "West Texas Intermediate (WTI) - Cushing, Oklahoma (DCOILWTICO) from FRED and Bently.",
         "Manufacturers' new orders (NEWORDERS) from FRED.",
         "AA Financial Commercial Paper Rates from FRED.",
         "AA Financial Commercial Paper Yield Curve from FRED."
  )
}



shinyServer(
  function(input, output) {
    output$Employment <- renderPlot({

      output$caption <- renderText(updateText(as.integer(input$indicator)) )

      plotType(as.integer(input$indicator))
      
    })
    
  }
)
