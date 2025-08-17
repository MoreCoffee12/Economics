
# Loans + reserves to compare to deposits
if ( require_columns(df.data, c("TOTLNNSA", "WRESBAL") ) ){
  
  df.data$TOTLLNSA.PLUS.WRESBAL <- df.data$TOTLLNSA + df.data$WRESBAL
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "TOTLLNSA.PLUS.WRESBAL",
        string.source = "Calc",
        string.description = "Total Loans Plus All Reserves (TOTLLNSA + WRESBAL)",
        string.label.y = "Percent",
        float.expense.ratio = -1.00,
        date.series.start = as.Date(max(
          c(df.symbols$date.series.start[df.symbols$string.symbol == 'TOTLLNSA'], index(WRESBAL[1]))
        )) ,
        date.series.end = as.Date(min(c(
          df.symbols$date.series.end[df.symbols$string.symbol == 'TOTLLNSA'], index(tail(WRESBAL, 1))
        )))
        
      )
    )
}

# Compare GDP growth the 1-year treasury
if ( require_columns(df.data, c("GDP_YoY", "DGS1") ) ){
  
  df.data$GDP_YoYTODGS1 <- df.data$GDP_YoY - df.data$DGS1
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "GDP_YoYTODGS1",
        string.source = "Calc",
        string.description = "Economic Yield Curve, GDP YoY and 1-Year Tr (GDP_YoY-DGS1)",
        string.label.y = "Percent",
        float.expense.ratio = -1.00,
        date.series.start = as.Date(max(
          c(df.symbols$date.series.start[df.symbols$string.symbol == 'GDP_YoY'], index(DGS1[1]))
        )) ,
        date.series.end = as.Date(min(c(
          df.symbols$date.series.end[df.symbols$string.symbol == 'GDP_YoY'], index(tail(DGS1, 1))
        )))
      )
    )
}

if ( require_columns(df.data, c("GDP_YoY", "TB3MS") ) ){
  
  df.data$GDP_YoYTOTB3MS <- df.data$GDP_YoY - df.data$TB3MS
  
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "GDP_YoYTOTB3MS",
        string.source = "Calc",
        string.description = "Economic Yield Curve, GDP YoY and 3-Month Tr (GDP_YoY-TB3MS)",
        string.label.y = "Percent",
        float.expense.ratio = -1.00,
        date.series.start = as.Date(max(
          c(df.symbols$date.series.start[df.symbols$string.symbol == 'GDP_YoY'], index(TB3MS[1]))
        )) ,
        date.series.end = as.Date(min(c(
          df.symbols$date.series.end[df.symbols$string.symbol == 'GDP_YoY'], index(tail(TB3MS, 1))
        )))
      )
    )
}

if ( require_columns(df.data, c("OPHNFB_YoY", "DGS1") ) ){
  
  df.data$OPHNFB_YoYTODGS1 <- df.data$OPHNFB_YoY - df.data$DGS1
  
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "OPHNFB_YoYTODGS1",
        string.source = "Calc",
        string.description = "Productivity Yield Curve, Real output YoY and 1-Year Tr (OPHNFB_YoY-DGS1)",
        string.label.y = "Percent",
        float.expense.ratio = -1.00,
        date.series.start = as.Date(max(
          c(df.symbols$date.series.start[df.symbols$string.symbol == 'OPHNFB_YoY'], index(DGS1[1]))
        )) ,
        date.series.end = as.Date(min(c(
          df.symbols$date.series.end[df.symbols$string.symbol == 'OPHNFB_YoY'], index(tail(DGS1, 1))
        )))
      )
    )
}

# S&P 500 Open divided by the 200 day SMA
if ( require_columns(df.data, c("GSPC.Open", "GSPC.Open_mva200") ) ){
  
df.data$GSPC.Open_mva200_Norm <-
  100 * (df.data$GSPC.Open / df.data$GSPC.Open_mva200)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GSPC.Open_mva200_Norm",
      string.source = "Calc",
      string.description = "S&P 500 normalized by 200 SMA ",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date(max(
        c(
          df.symbols$date.series.start[df.symbols$string.symbol == 'GSPC.Open'],
          df.symbols$date.series.start[df.symbols$string.symbol == 'GSPC.Open_mva200']
        )
      )) ,
      date.series.end = as.Date(min(
        c(df.symbols$date.series.end[df.symbols$string.symbol == 'GSPC.Open'],
          df.symbols$date.series.end[df.symbols$string.symbol == 'GSPC.Open_mva200'])
      ))
    )
  )

}

# S&P 200 day SMA minus 50 day SMA
if ( require_columns(df.data, c("GSPC.Open_mva050", "GSPC.Open_mva200") ) ){
  
  df.data$GSPC.Open_mva050_mva200 <-
    df.data$GSPC.Open_mva050 - df.data$GSPC.Open_mva200
  
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "GSPC.Open_mva050_mva200",
        string.source = "Calc",
        string.description = "S&P 500 50 SMA - 200 SMA ",
        string.label.y = "Dollars",
        float.expense.ratio = -1.00,
        date.series.start = as.Date(max(
          c(
            df.symbols$date.series.start[df.symbols$string.symbol == 'GSPC.Open_mva050'],
            df.symbols$date.series.start[df.symbols$string.symbol == 'GSPC.Open_mva200']
          )
        )) ,
        date.series.end = as.Date(min(
          c(df.symbols$date.series.end[df.symbols$string.symbol == 'GSPC.Open_mva050'],
            df.symbols$date.series.end[df.symbols$string.symbol == 'GSPC.Open_mva200'])
        ))
      )
    )

}

# Trading signal based on S&P 50 day SMA minus 200 day SMA
if ( require_columns(df.data, c("GSPC.Open_mva050_mva200") ) ){
  
  df.data$GSPC.Open_mva050_mva200_sig <-
    as.numeric(df.data$GSPC.Open_mva050_mva200 > 0)
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "GSPC.Open_mva050_mva200_sig",
        string.source = "Calc",
        string.description = "Sell Signal S&P 500 50 SMA - 200 SMA ",
        string.label.y = "-",
        float.expense.ratio = -1.00,
        date.series.start = as.Date(df.symbols$date.series.start[df.symbols$string.symbol == 'GSPC.Open_mva050_mva200']) ,
        date.series.end = as.Date(df.symbols$date.series.end[df.symbols$string.symbol == 'GSPC.Open_mva050_mva200'])
      )
    )
}

# These are dashboard metrics
getRecentHighCol <- function(iDays) {
  strColMaxName <- paste("Max", sprintf("%03.0f", iDays), sep = '')
  return(strColMaxName)
  
}


#' Get Recent highs
#'
#' @param df.data
#' @param df.symbols
#' @param iDays
#' @param strName
#'
#' @return
#' @export
#'
#' @examples
getRecentHigh <- function(df.data, df.symbols, iDays, strName) {
  
  strColMaxName <- getRecentHighCol(iDays)
  
  iLastRow = nrow(df.data)
  #df.symbols[strColMaxName] <- FALSE
  
  dCurrent <- df.data[iLastRow, strName]
  
  # Execute if it is a number
  if (!is.na(dCurrent) && strName != 'date') {
    
    dfTestPoint <- tail(df.data[strName], iDays)
    #print(paste("dfTestPoint: ",dfTestPoint))
    
    dfMax = dfTestPoint[dfTestPoint[, 1] > dCurrent[1],]
    #print(paste("dfMax: ",dfMax))
    
    # If the test point is valid
    df.symbols[df.symbols$string.symbol == strName, strColMaxName] <-
      FALSE
    if (!any(is.na(dfTestPoint)) && is.numeric(dfMax)) {
      if (length(dfMax) == 0) {
        #print("TRUE")
        df.symbols[df.symbols$string.symbol == strName, strColMaxName] <-
          TRUE
      }
    }
  }
  
  return(df.symbols)
  
}

# Setup the 30 day and 180 day highs
for (strName in names(df.data)) {
  #strName = "DGS1"
  
  #print(strName)
  
  iDays <- 30
  df.symbols <- getRecentHigh(df.data, df.symbols, iDays, strName)
  
  iDays <- 180
  df.symbols <- getRecentHigh(df.data, df.symbols, iDays, strName)
}

# This speeds some of the upcoming operations
df.data$date = as.Date(rownames(df.data))


# These are auxiliary series that will be used a bit later.

# Add a smoothed U-3 unemployment rate with 21 day kernel
if ( require_columns(df.data, c("UNRATE") ) ){
  
  df.data$UNRATE_Smooth <-
    sgolayfilt(
      df.data$UNRATE,
      p = 3,
      n = 21,
      m = 0,
      ts = 1
    )
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "UNRATE_Smooth_21",
        string.source = "Calc",
        string.description =  "Smoothed Civilian Unemployment Rate U-3",
        string.label.y = "Percent",
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(index(UNRATE[1])),
        date.series.end = as.Date(index(tail(UNRATE, 1)))
      )
    )
}

# Second derivative of the U-3 unemployment rate
if ( require_columns(df.data, c("UNRATE") ) ){
  
  df.data$UNRATE_SmoothDer2 <-
    sgolayfilt(
      df.data$UNRATE,
      p = 3,
      n = 501,
      m = 2,
      ts = 1
    )
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "UNRATE_SmoothDer2",
        string.source = "Calc",
        string.description =  "2nd Derivative of Smoothed U-3",
        string.label.y = "Percent/period/period" ,
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(index(UNRATE[1])),
        date.series.end = as.Date(index(tail(UNRATE, 1)))
      )
    )
}

# Add a smoothed U-6 unemployement rate with 21 day kernel
if ( require_columns(df.data, c("U6RATE") ) ){
  
  df.data$U6RATE_Smooth <-
    sgolayfilt(
      df.data$U6RATE,
      p = 3,
      n = 21,
      m = 0,
      ts = 1
    )
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "U6RATE_Smooth_21",
        string.source = "Calc",
        string.description =  "Smoothed Total Unemployed U-6",
        string.label.y = "Percent",
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(index(U6RATE[1])),
        date.series.end = as.Date(index(tail(U6RATE, 1)))
      )
    )
}

# Second derivative of the U-6 unemployment rate
if ( require_columns(df.data, c("U6RATE") ) ){
  
  df.data$U6RATE_SmoothDer2 <-
    sgolayfilt(
      df.data$U6RATE,
      p = 3,
      n = 501,
      m = 2,
      ts = 1
    )
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "UNRATE_SmoothDer2",
        string.source = "Calc",
        string.description =  "2nd Derivative of Smoothed U-6",
        string.label.y = "Percent/period/period" ,
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(index(U6RATE[1])),
        date.series.end = as.Date(index(tail(U6RATE, 1)))
      )
    )
}

# Smoothed derivative of the S&P 500 log values
if ( require_columns(df.data, c("GSPC.Open_Log") ) ){
  
  df.data$GSPC.Open_Log_SmoothDer <-
    sgolayfilt(
      df.data$GSPC.Open_Log,
      p = 3,
      n = 501,
      m = 1,
      ts = 1
    )
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "GSPC.Open_Log_SmoothDer",
        string.source = "Calc",
        string.description =  "Derivative of Smoothed Log Scale S&P 500",
        string.label.y = "Dollar/period",
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(index(GSPC[1])),
        date.series.end = as.Date(index(tail(GSPC, 1)))
      )
    )
}

# Smoothed derivative of the S&P 500 log values, normalized by the GDP deflator
if ( require_columns(df.data, c("GSPC.Open.by.GDPDEF_Log") ) ){
  
  df.data$GSPC.Open.by.GDPDEF_Log_SmoothDer <-
    sgolayfilt(
      df.data$GSPC.Open.by.GDPDEF_Log,
      p = 3,
      n = 501,
      m = 1,
      ts = 1
    )
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "GSPC.Open.by.GDPDEF_Log_SmoothDer",
        string.source = "Calc",
        string.description =  "Derivative of Smoothed Log Scale S&P 500\ndivided by GDP deflator",
        string.label.y = "Dollar/period",
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(index(GSPC[1])),
        date.series.end = as.Date(index(tail(GSPC, 1)))
      )
    )
}


# Smoothed second derivative of the S&P 500 log values
if ( require_columns(df.data, c("GSPC.Open_Log_SmoothDer") ) ){
    
  df.data$GSPC.Open_Log_SmoothDerDer <-
    sgolayfilt(
      df.data$GSPC.Open_Log_SmoothDer,
      p = 3,
      n = 501,
      m = 1,
      ts = 1
    )
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "GSPC.Open_Log_SmoothDerDer",
        string.source = "Calc",
        string.description =  "Derivative of Smoothed Log Scale S&P 500",
        string.label.y = "Dollar/period/period",
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(index(GSPC[1])),
        date.series.end = as.Date(index(tail(GSPC, 1)))
      )
    )

}
  
# Smoothed business debt levels
if ( require_columns(df.data, c("NCBDBIQ027S_Log") ) ){
    
  df.data$NCBDBIQ027S_Log_Der <-
    sgolayfilt(
      df.data$NCBDBIQ027S_Log,
      p = 3,
      n = 501,
      m = 1,
      ts = 1
    )
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "NCBDBIQ027S_Log_Der",
        string.source = "Calc",
        string.description =  "Derivative of Smoothed Log Nonfinancial corporate business; debt securities; liability, Level",
        string.label.y = "log(Milllons of Dollars)/period" ,
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(index(NCBDBIQ027S[1])),
        date.series.end = as.Date(index(tail(NCBDBIQ027S, 1)))
      )
    )

}
  
  
# Smoothed business loan levels
if ( require_columns(df.data, c("BUSLOANS_Log") ) ){
  
  df.data$BUSLOANS_Log_Der <-
    sgolayfilt(
      df.data$BUSLOANS_Log,
      p = 3,
      n = 501,
      m = 1,
      ts = 1
    )
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "BUSLOANS_Log_Der",
        string.source = "Calc",
        string.description =  "Derivative of Smoothed Log Commercial and Industrial Loans",
        string.label.y = "log(Billlons of U.S. Dollars)/period" ,
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(index(BUSLOANS[1])),
        date.series.end = as.Date(index(tail(BUSLOANS, 1)))
      )
    )
}


# Smoothed gross private domestic investment
if ( require_columns(df.data, c("GPDI_Log") ) ){
  
  df.data$GPDI_Log_Der <-
    sgolayfilt(
      df.data$GPDI_Log,
      p = 3,
      n = 501,
      m = 1,
      ts = 1
    )
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "GPDI_Log_Der",
        string.source = "FRED",
        string.description =  "Gross Private Domestic Investment",
        string.label.y = "log(Billions of Dollars)",
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(index(GPDI[1])),
        date.series.end = as.Date(index(tail(GPDI, 1)))
      )
    )
}


# Create the ratio of S&P 500 close to GDP
if ( require_columns(df.data, c("GSPC.Close", "GDP") ) ){
    
  df.data$GDPSP500 <- df.data$GSPC.Close / df.data$GDP
  
  df.data$GDPSP500 <- na.approx(df.data$GDPSP500, rule = 2)
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "GDPSP500",
        string.source = "Ratio",
        string.description =  "S&P 500 (GSPC.Close)/GDP",
        string.label.y = "Ratio ($/$)" ,
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(max(c(
          index(GSPC[1]), index(GDP[1])
        ))) ,
        date.series.end = as.Date(min(c(
          index(tail(GSPC, 1)), index(tail(GDP, 1))
        )))
      )
    )

}

# Create the ratio of Russell 2000 close to GDP
if ( require_columns(df.data, c("RLG.Close", "GDP") ) ){
  
  df.data$RLGSP500 <- df.data$RLG.Close / df.data$GDP
  df.data$RLGSP500 <- na.approx(df.data$RLGSP500, rule = 2)
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "RLGSP500",
        string.source = "Ratio",
        string.description =  "Russell 2000 (RLG.Close)/GDP",
        string.label.y = "Ratio ($/$)" ,
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(max(c(
          index(RLG[1]), index(GDP[1])
        ))) ,
        date.series.end = as.Date(min(c(
          index(tail(RLG, 1)), index(tail(GDP, 1))
        )))
      )
    )
}

# Create the ratio of Dow Jones industrial average close to GDP
if ( require_columns(df.data, c("DJI.Close", "GDP") ) ){
  
  df.data$DJISP500 <- df.data$DJI.Close / df.data$GDP
  
  df.data$DJISP500 <- na.approx(df.data$DJISP500, rule = 2)
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "DJISP500",
        string.source = "Ratio",
        string.description =  "Dow Jones (DJI.Close)/GDP",
        string.label.y = "Ratio ($/$)" ,
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(max(c(
          index(DJI[1]), index(GDP[1])
        ))) ,
        date.series.end = as.Date(min(c(
          index(tail(DJI, 1)), index(tail(GDP, 1))
        )))
      )
    )

}


# This is the NY Fed's model for recession basedon the 10 y to 3 month spread
if ( require_columns(df.data, c("DGS10TOTB3MS") ) ){
  
  nyfed.alpha = -0.5333
  nyfed.beta = -0.6330
  #df.data$nyfed.recession <- shift(pnorm(nyfed.beta + df.data$DGS10TOTB3MS*nyfed.alpha), n=360, fill = 0)
  df.data$nyfed.recession <-
    pnorm(nyfed.beta + df.data$DGS10TOTB3MS * nyfed.alpha)
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "nyfed.recession",
        string.source = "Calc",
        string.description =  "Probability of US Recession Predicted by Treasury Spread (12 month)",
        string.label.y = "-",
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(df.symbols$date.series.start[df.symbols$string.symbol == 'DGS10TOTB3MS']),
        date.series.end = as.Date(df.symbols$date.series.end[df.symbols$string.symbol == 'DGS10TOTB3MS'])
      )
    )

}

# Gross Private Domestic Investment to GDP
if ( require_columns(df.data, c("GDP") ) ){
  
  df.data$GPDI.by.GDP <- df.data$GPDI / df.data$GDP
  df.data$GPDI.by.GDP <- na.approx(df.data$GPDI.by.GDP, rule = 2)
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "GPDI.by.GDP",
        string.source = "Ratio",
        string.description =  "Gross Private Domestic Investment/GDP",
        string.label.y = "Ratio ($/$)" ,
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = as.Date(max(c(
          index(GPDI[1]), index(GDP[1])
        ))) ,
        date.series.end = as.Date(min(c(
          index(tail(GPDI, 1)), index(tail(GDP, 1))
        )))
      )
    )
}

#--------------------------------------------------------------------------
# Calculate returns
#--------------------------------------------------------------------------

# returns for base case (S&P 500)
if ( require_columns(df.data, c("GSPC.Close") ) ){
  
  df.data$retBase <- ROC(df.data$GSPC.Close)
  df.data$retBase[is.na(df.data$retBase)] <- 0
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "retBase",
        string.source = "Calc",
        string.description =  "S&P 500 Rate of Change",
        string.label.y = "Percent",
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = dt.start.prediction ,
        date.series.end = as.Date(Sys.Date())
      )
    )

}

# returns for 3-month t-bills (assumed to be short position)
if ( require_columns(df.data, c("TB3MS") ) ){
  
  df.data$retBaseShort_TB3MS <- df.data$TB3MS / 365
  df.data$retBaseShort_TB3MS[is.na(df.data$retBaseShort_TB3MS)] <- 0
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "retBaseShort_TB3MS",
        string.source = "Calc",
        string.description =  "retBaseShort_TB3MS Rate of Change",
        string.label.y = "Percent",
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = dt.start.prediction ,
        date.series.end = as.Date(Sys.Date())
      )
    )

}

# Growth for base case (S&P 500)
if ( require_columns(df.data, c("retBase") ) ){
  
  df.data$eqBase <- exp(cumsum(df.data$retBase))
  df.data$eqBase <-
    df.data$eqBase / df.data[min(which(df.data$date > dtStartBackTest)), "eqBase"]
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "eqBase",
        string.source = "Calc",
        string.description =  "Equity Return, 100% long",
        string.label.y = "$1 Invested",
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = dt.start.prediction ,
        date.series.end = as.Date(Sys.Date())
      )
    )
}


# Growth for 3-month t-bill
if ( require_columns(df.data, c("retBaseShort_TB3MS") ) ){
    
  df.data$eqBaseShort_TB3MS <- cumsum(df.data$retBaseShort_TB3MS)
  df.data$eqBaseShort_TB3MS <-
    df.data$eqBaseShort_TB3MS / df.data[min(which(df.data$date > dtStartBackTest)), "eqBaseShort_TB3MS"]
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "eqBaseShort_TB3MS",
        string.source = "Calc",
        string.description =  "3-Month t-Bill Return, 100% long",
        string.label.y = "$1 Invested",
        float.expense.ratio = -1.00,
        Max030 = FALSE,
        Max180 = FALSE,
        date.series.start = dt.start.prediction,
        date.series.end = as.Date(Sys.Date())
      )
    )
}
