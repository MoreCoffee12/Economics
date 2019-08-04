
# Wrapper to pull in external data
getExternalData <- (dfSyms){

  # This pulls in data using quantmod
  for (idx in 1:nrow(dfSyms)){
    getSymbols(as.character(dfSyms[idx,"Symbol"]),
               src=as.character(dfSyms[idx,"Source"]), 
               auto.assign=TRUE, 
               from = as.Date("1945-01-01"), to = Sys.Date())
  }

  
  # There are a few symbols I wasn't able to include with quantmod so this section uses Quandl to get those data pieces.
  # API key 6gea4eLup_eVmnxX9DPA
  dfSyms <- rbind(dfSyms, data.frame(Symbol="ISM/MAN_PMI", Source="QUANDL", 
                                     Desc="Institute of Supply Managment PMI Composite Index",
                                     yLabel = "Index" ) )
  
  Quandl.api_key('d9LUhcBVPa_8MFdtiFda')
  dfPMIComp <- oil_daily <- Quandl("ISM/MAN_PMI", type = "raw", collapse = "daily",  
                                   start_date="1910-01-01", end_date=Sys.Date())
  strName <- str_replace_all(tail(dfSyms,1)$Symbol, "[^[:alnum:]]", "")
  assign(strName,xts(dfPMIComp[,-1], order.by=dfPMIComp[,1]))
  colnames(ISMMANPMI) <- c(strName)
  
  strQuandl = "MULTPL/SP500_PE_RATIO_MONTH";
  dfSyms <- rbind(dfSyms, data.frame(Symbol=strQuandl, Source="QUANDL", 
                                     Desc="S&P 500 TTM P/E",
                                     yLabel = "Index" ) )
  dfTemp <- oil_daily <- Quandl(strQuandl, type = "raw", collapse = "daily",  
                                start_date="1910-01-01", end_date=Sys.Date())
  
  strName <- str_replace_all(tail(dfSyms,1)$Symbol, "[^[:alnum:]]", "")
  assign(strName,xts(dfTemp[,-1], order.by=dfTemp[,1]))
  colnames(MULTPLSP500PERATIOMONTH) <- c(strName)
  
  strQuandl = "MULTPL/SP500_SALES_QUARTER";
  dfSyms <- rbind(dfSyms, data.frame(Symbol=strQuandl, Source="QUANDL", 
                                     Desc="S&P 500 TTM Sales (Not Inflation Adjusted)",
                                     yLabel = "Index" ) )
  dfTemp <- oil_daily <- Quandl(strQuandl, type = "raw", collapse = "daily",  
                                start_date="1910-01-01", end_date=Sys.Date())
  
  strName <- str_replace_all(tail(dfSyms,1)$Symbol, "[^[:alnum:]]", "")
  assign(strName,xts(dfTemp[,-1], order.by=dfTemp[,1]))
  colnames(MULTPLSP500SALESQUARTER) <- c(strName)
  
  strQuandl = "NYXDATA/MARKET_CREDIT";
  dfSyms <- rbind(dfSyms, data.frame(Symbol=strQuandl, Source="QUANDL", 
                                     Desc="NYSE Margin Debt",
                                     yLabel = "Dollars" ) )
  dfTemp <- oil_daily <- Quandl(strQuandl, type = "raw", collapse = "daily",  
                                start_date="1910-01-01", end_date=Sys.Date())
  
  strName <- str_replace_all(tail(dfSyms,1)$Symbol, "[^[:alnum:]]", "")
  assign(strName,xts(dfTemp[,-1], order.by=dfTemp[,1]))
  colnames(NYXDATAMARKETCREDIT)[[1]] <- c(strName)
  
  
}


