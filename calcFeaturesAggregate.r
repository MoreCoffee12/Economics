#------------ Extrace features from the aggregate series


# Loans + reserves to compare to deposits
if ( require_columns(df.data, c("TOTLNNSA", "WRESBAL") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "TOTLLNSA__plus__WRESBAL"
  df.data[[str.symbol.new]] <- ( df.data$TOTLLNSA + df.data$WRESBAL )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "Total Loans Plus All Reserves (TOTLLNSA + WRESBAL)",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date(max(
        c(df.symbols$date.series.start[df.symbols$string.symbol == 'TOTLLNSA'], index(WRESBAL[1]))
      )) ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$string.symbol == 'TOTLLNSA'], index(tail(WRESBAL, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
      )
  )  

  # Tidy memory
  rm(str.symbol.new)
  
}

# Compare GDP growth the 1-year treasury
if ( require_columns(df.data, c("GDP_YoY", "DGS1") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GDP__YoYTODGS1"
  df.data[[str.symbol.new]] <- ( df.data$GDP_YoY - df.data$DGS1 )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "Economic Yield Curve, GDP YoY and 1-Year Tr (GDP_YoY-DGS1)",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date(max(
        c(df.symbols$date.series.start[df.symbols$string.symbol == 'GDP_YoY'], index(DGS1[1]))
      )) ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$string.symbol == 'GDP_YoY'], index(tail(DGS1, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)  

}

if ( require_columns(df.data, c("GDP_YoY", "TB3MS") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GDP__YoYTOTB3MS"
  df.data[[str.symbol.new]] <- ( df.data$GDP_YoY - df.data$TB3MS )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "Economic Yield Curve, GDP YoY and 3-Month Tr (GDP_YoY-TB3MS)",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date(max(
        c(df.symbols$date.series.start[df.symbols$string.symbol == 'GDP_YoY'], index(TB3MS[1]))
      )) ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$string.symbol == 'GDP_YoY'], index(tail(TB3MS, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new) 
  
}

if ( require_columns(df.data, c("OPHNFB_YoY", "DGS1") ) ){
  
  # Define the new symbol and make the calculation
  str.symbol.new <- "OPHNFB__YoYTODGS1"
  df.data[[str.symbol.new]] <- (  df.data$OPHNFB_YoY - df.data$DGS1 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "Productivity Yield Curve, Real output YoY and 1-Year Tr (OPHNFB_YoY-DGS1)",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date(max(
        c(df.symbols$date.series.start[df.symbols$string.symbol == 'OPHNFB_YoY'], index(DGS1[1]))
      )) ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$string.symbol == 'OPHNFB_YoY'], index(tail(DGS1, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new) 

}

# S&P 500 Open divided by the 200 day SMA
if ( require_columns(df.data, c("GSPC.Open", "GSPC.Open_mva200") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GSPC__Open__mva200__Norm"
  df.data[[str.symbol.new]] <- 
    ( 100.0  * ( df.data$GSPC.Open / df.data$GSPC.Open_mva200 ) )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      )),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}

# S&P 200 day SMA minus 50 day SMA
if ( require_columns(df.data, c("GSPC.Open_mva050", "GSPC.Open_mva200") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GSPC__Open__mva050__mva200"
  df.data[[str.symbol.new]] <- 
    df.data$GSPC.Open_mva050 - df.data$GSPC.Open_mva200

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      )),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Trading signal based on S&P 50 day SMA minus 200 day SMA
if ( require_columns(df.data, c("GSPC.Open_mva050_mva200") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GSPC__Open__mva050__mva200__sig"
  df.data[[str.symbol.new]] <-
    as.numeric( df.data$GSPC.Open_mva050_mva200 > 0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "Sell Signal S&P 500 50 SMA - 200 SMA ",
      string.label.y = "-",
      float.expense.ratio = -1.00,
      date.series.start = as.Date(df.symbols$date.series.start[df.symbols$string.symbol == 'GSPC.Open_mva050_mva200']) ,
      date.series.end = as.Date(df.symbols$date.series.end[df.symbols$string.symbol == 'GSPC.Open_mva050_mva200']),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

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

  # Define the new symbol and make the calculation
  str.symbol.new <- "UNRATE__Smooth__21"
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data$UNRATE,
      p = 3,
      n = 21,
      m = 0,
      ts = 1
    )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "Smoothed Civilian Unemployment Rate U-3",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(UNRATE[1])),
      date.series.end = as.Date(index(tail(UNRATE, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}

# Second derivative of the U-3 unemployment rate
if ( require_columns(df.data, c("UNRATE") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "UNRATE__SmoothDer2"
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data$UNRATE,
      p = 3,
      n = 501,
      m = 2,
      ts = 1
    )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "2nd Derivative of Smoothed U-3",
      string.label.y = "Percent/period/period" ,
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(UNRATE[1])),
      date.series.end = as.Date(index(tail(UNRATE, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}

# Add a smoothed U-6 unemployement rate with 21 day kernel
if ( require_columns(df.data, c("U6RATE") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "U6RATE__Smooth__21"
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data$U6RATE,
      p = 3,
      n = 21,
      m = 0,
      ts = 1
    )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "Smoothed Total Unemployed U-6",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(U6RATE[1])),
      date.series.end = as.Date(index(tail(U6RATE, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Second derivative of the U-6 unemployment rate
if ( require_columns(df.data, c("U6RATE") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "U6RATE__SmoothDer2"
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data$U6RATE,
      p = 3,
      n = 501,
      m = 2,
      ts = 1
    )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "2nd Derivative of Smoothed U-6",
      string.label.y = "Percent/period/period" ,
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(U6RATE[1])),
      date.series.end = as.Date(index(tail(U6RATE, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}

# Smoothed derivative of the S&P 500 log values
if ( require_columns(df.data, c("GSPC.Open_Log") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GSPC__Open__Log__SmoothDer"
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data$GSPC.Open_Log,
      p = 3,
      n = 501,
      m = 1,
      ts = 1
    )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "Derivative of Smoothed Log Scale S&P 500",
      string.label.y = "Dollar/period",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(GSPC[1])),
      date.series.end = as.Date(index(tail(GSPC, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Smoothed derivative of the S&P 500 log values, normalized by the GDP deflator
if ( require_columns(df.data, c("GSPC.Open.by.GDPDEF_Log") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GSPC__Open__by__GDPDEF__Log__SmoothDer"
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data$GSPC.Open.by.GDPDEF_Log,
      p = 3,
      n = 501,
      m = 1,
      ts = 1
    )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "Derivative of Smoothed Log Scale S&P 500\ndivided by GDP deflator",
      string.label.y = "Dollar/period",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(GSPC[1])),
      date.series.end = as.Date(index(tail(GSPC, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}


# Smoothed second derivative of the S&P 500 log values
if ( require_columns(df.data, c("GSPC.Open_Log_SmoothDer") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GSPC__Open__Log__SmoothDerDer"
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data$GSPC__Open__Log__SmoothDer,
      p = 3,
      n = 501,
      m = 1,
      ts = 1
    )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "Derivative of Smoothed Log Scale S&P 500",
      string.label.y = "Dollar/period/period",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(GSPC[1])),
      date.series.end = as.Date(index(tail(GSPC, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)
  
}
  
# Smoothed business debt levels
if ( require_columns(df.data, c("NCBDBIQ027S_Log") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "NCBDBIQ027S__Log__Der"
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data$NCBDBIQ027S__Log,
      p = 3,
      n = 501,
      m = 1,
      ts = 1
    )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "Derivative of Smoothed Log Nonfinancial corporate business; debt securities; liability, Level",
      string.label.y = "log(Milllons of Dollars)/period" ,
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(NCBDBIQ027S[1])),
      date.series.end = as.Date(index(tail(NCBDBIQ027S, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}
  
  
# Smoothed business loan levels
if ( require_columns(df.data, c("BUSLOANS_Log") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "BUSLOANS__Log__Der"
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data$BUSLOANS__Log,
      p = 3,
      n = 501,
      m = 1,
      ts = 1
    )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "Derivative of Smoothed Log Commercial and Industrial Loans",
      string.label.y = "log(Billlons of U.S. Dollars)/period" ,
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(BUSLOANS[1])),
      date.series.end = as.Date(index(tail(BUSLOANS, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Smoothed gross private domestic investment
if ( require_columns(df.data, c("GPDI_Log") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GPDI__Log__Der"
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data$GPDI_Log,
      p = 3,
      n = 501,
      m = 1,
      ts = 1
    )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "FRED",
      string.description =  "Gross Private Domestic Investment",
      string.label.y = "log(Billions of Dollars)",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(GPDI[1])),
      date.series.end = as.Date(index(tail(GPDI, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Create the ratio of S&P 500 close to GDP
if ( require_columns(df.data, c("GSPC.Close", "GDP") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GDPSP500"
  df.data[[str.symbol.new]] <- ( df.data$GSPC.Close / df.data$GDP )
  df.data[[str.symbol.new]] <- na.approx(df.data[[str.symbol.new]], rule = 2)
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}

# Create the ratio of Russell 2000 close to GDP
if ( require_columns(df.data, c("RLG.Close", "GDP") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GDPSP500"
  df.data[[str.symbol.new]] <- ( df.data$RLG.Close / df.data$GDP )
  df.data[[str.symbol.new]] <- na.approx(df.data[[str.symbol.new]], rule = 2)
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Create the ratio of Dow Jones industrial average close to GDP
if ( require_columns(df.data, c("DJI.Close", "GDP") ) ){
  
  # Define the new symbol and make the calculation
  str.symbol.new <- "DJISP500"
  df.data[[str.symbol.new]] <- df.data$DJI.Close / df.data$GDP
  df.data[[str.symbol.new]] <- na.approx(df.data[[str.symbol.new]], rule = 2)

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}


# This is the NY Fed's model for recession basedon the 10 y to 3 month spread
if ( require_columns(df.data, c("DGS10TOTB3MS") ) ){
  
  nyfed.alpha = -0.5333
  nyfed.beta = -0.6330
  
  # Define the new symbol and make the calculation
  str.symbol.new <- "nyfed.recession"
  df.data[[str.symbol.new]] <-
    ( pnorm(nyfed.beta + df.data$DGS10TOTB3MS * nyfed.alpha) )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "Probability of US Recession Predicted by Treasury Spread (12 month)",
      string.label.y = "-",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(df.symbols$date.series.start[df.symbols$string.symbol == 'DGS10TOTB3MS']),
      date.series.end = as.Date(df.symbols$date.series.end[df.symbols$string.symbol == 'DGS10TOTB3MS']),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}

# Gross Private Domestic Investment to GDP
if ( require_columns(df.data, c("GDP", "GDPI") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GPDI__by__GDP"
  df.data[[str.symbol.new]] <- ( df.data$GPDI / df.data$GDP )
  df.data[[str.symbol.new]] <- na.approx(df.data[[str.symbol.new]], rule = 2)
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}

#--------------------------------------------------------------------------
# Calculate returns
#--------------------------------------------------------------------------

# returns for base case (S&P 500)
if ( require_columns(df.data, c("GSPC.Close") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "retBase"
  df.data[[str.symbol.new]] <- ROC(df.data$GSPC.Close)
  df.data[[str.symbol.new]][is.na(df.data[[str.symbol.new]])] <- 0
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "S&P 500 Rate of Change",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = dt.start.prediction ,
      date.series.end = as.Date(Sys.Date()),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}

# returns for 3-month t-bills (assumed to be short position)
if ( require_columns(df.data, c("TB3MS") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "retBaseShort__TB3MS"
  df.data[[str.symbol.new]] <- df.data$TB3MS / 365
  df.data[[str.symbol.new]][is.na(df.data[[str.symbol.new]])] <- 0

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "retBaseShort_TB3MS Rate of Change",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = dt.start.prediction ,
      date.series.end = as.Date(Sys.Date()),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}

# Growth for base case (S&P 500)
if ( require_columns(df.data, c("retBase") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "eqBase"
  df.data[[str.symbol.new]] <- exp(cumsum(df.data$retBase))
  df.data[[str.symbol.new]] <-
    df.data[[str.symbol.new]] / df.data[min(which(df.data$date > dtStartBackTest)), str.symbol.new]

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "Equity Return, 100% long",
      string.label.y = "$1 Invested",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = dt.start.prediction ,
      date.series.end = as.Date(Sys.Date()),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}


# Growth for 3-month t-bill
if ( require_columns(df.data, c("retBaseShort_TB3MS") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "eqBaseShort_TB3MS"
  df.data[[str.symbol.new]] <- cumsum(df.data$retBaseShort_TB3MS)
  df.data[[str.symbol.new]] <-
    df.data[[str.symbol.new]] / df.data[min(which(df.data$date > dtStartBackTest)), str.symbol.new]
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description =  "3-Month t-Bill Return, 100% long",
      string.label.y = "$1 Invested",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = dt.start.prediction,
      date.series.end = as.Date(Sys.Date()),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}
