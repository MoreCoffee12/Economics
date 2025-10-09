#------------ Extract features from the aggregate series


# Loans + reserves to compare to deposits
lst.sym <- c("TOTLLNSA.Value", "WRESBAL.Value")
if ( require_columns(df.data, lst.sym ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "TOTLLNSA__plus__WRESBAL"
  df.data[[str.symbol.new]] <- 
    ( df.data[[lst.sym[[1]]]] + df.data[[lst.sym[[2]]]] )

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

# Free up memory
rm(lst.sym)

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
if ( require_columns(df.data, c("X_GSPC.GSPC.Open", "X_GSPC.GSPC.Open__mva200") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GSPC__Open__mva200__Norm"
  df.data[[str.symbol.new]] <- 
    ( 100.0  * ( df.data$X_GSPC.GSPC.Open / df.data$X_GSPC.GSPC.Open__mva200 ) )

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
lst.syms <- c("X_GSPC.GSPC.Open__mva050", "X_GSPC.GSPC.Open__mva200")
if ( require_columns(df.data, lst.syms ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GSPC__Open__mva050__minus__mva200"
  df.data[[str.symbol.new]] <- 
    df.data[[lst.syms[[1]]]] - df.data[[lst.syms[[2]]]]

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
          df.symbols$date.series.start[df.symbols$string.symbol == lst.syms[[1]]],
          df.symbols$date.series.start[df.symbols$string.symbol == lst.syms[[2]]]
        )
      )) ,
      date.series.end = as.Date(min(
        c(df.symbols$date.series.end[df.symbols$string.symbol == lst.syms[[1]]],
          df.symbols$date.series.end[df.symbols$string.symbol == lst.syms[[2]]])
      )),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Tidy memory
rm(lst.syms)

# Trading signal based on S&P 50 day SMA minus 200 day SMA
lst.syms <- c("GSPC__Open__mva050__minus__mva200")
if ( require_columns(df.data, lst.syms) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GSPC__Open__mva050__mva200__sig"
  df.data[[str.symbol.new]] <-
    as.numeric( df.data[[lst.syms[[1]]]] > 0 )
  
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

# Tidy memory
rm(lst.syms)

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
list.sym <- c("X_GSPC.GSPC.Open__Log")
if ( require_columns(df.data, list.sym ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GSPC__Open__Log__SmoothDer"
  
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data[[list.sym[[1]]]],
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
      date.series.start = as.Date(index(X_GSPC[1])),
      date.series.end = as.Date(index(tail(X_GSPC, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  if( exists("str.symbol.new")){
    rm(str.symbol.new)
  }

  
}else{
  print(paste("Failed to find: ", lst.syms))  
}

# Clean and tidy memory
if( exists("list.sym")){
  rm(list.sym)
}

# Smoothed derivative of the S&P 500 log values, normalized by the GDP deflator
list.sym <- c("X_GSPC__Open__by__GDPDEF__Log")
if ( require_columns(df.data, list.sym ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GSPC__Open__by__GDPDEF__Log__SmoothDer"
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data[[list.sym[[1]]]],
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
      string.description =  "Derivative of Smoothed Log Scale S&P 500 Open\ndivided by GDP deflator",
      string.label.y = "Dollar/period",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(X_GSPC[1])),
      date.series.end = as.Date(index(tail(X_GSPC, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}else{
  print(paste("Failed to find: ", lst.syms))  
}

# Clean and tidy memory
if( exists("list.sym")){
  rm(list.sym)
}

# Smoothed derivative of the S&P 500 log values, normalized by the GDP deflator
list.sym <- c("X_GSPC__Close__by__GDPDEF__Log")
if ( require_columns(df.data, list.sym ) ){
  
  # Define the new symbol and make the calculation
  str.symbol.new <- "GSPC__Close__by__GDPDEF__Log__SmoothDer"
  df.data[[str.symbol.new]] <-
    sgolayfilt(
      df.data[[list.sym[[1]]]],
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
      string.description =  "Derivative of Smoothed Log Scale S&P 500 Close\ndivided by GDP deflator",
      string.label.y = "Dollar/period",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(X_GSPC[1])),
      date.series.end = as.Date(index(tail(X_GSPC, 1))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  if( exists("str.symbol.new")){
    rm(str.symbol.new)
  }
  
}else{
  print(paste("Failed to find: ", lst.syms))  
}

# Clean and tidy memory
if( exists("list.sym")){
  rm(list.sym)
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
if ( require_columns(df.data, c("X_GSPC.GSPC.Close", "GDP.Value") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GDPSP500"
  df.data[[str.symbol.new]] <- ( df.data$X_GSPC.GSPC.Close / df.data$GDP.Value )
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
        index(X_GSPC[1]), index(GDP[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(X_GSPC, 1)), index(tail(GDP, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}

# Create the ratio of Russell 2000 close to GDP
if ( require_columns(df.data, c("X_RLG.RLG.Close", "GDP.Value") ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "RLGSP500"
  df.data[[str.symbol.new]] <- ( df.data$X_RLG.RLG.Close / df.data$GDP.Value )
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
        index(X_RLG[1]), index(GDP[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(X_RLG, 1)), index(tail(GDP, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Create the ratio of Dow Jones industrial average close to GDP
list.sym <- c("X_DJI.DJI.Close", "GDP.Value")
if ( require_columns(df.data, list.sym  ) ){
  
  # Define the new symbol and make the calculation
  str.symbol.new <- "DJISP500"
  df.data[[str.symbol.new]] <- df.data[[list.sym[[1]]]] / df.data[[list.sym[[2]]]]
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
        index(X_DJI[1]), index(GDP[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(X_DJI, 1)), index(tail(GDP, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
      
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)

}


# This is the NY Fed's model for recession based on the 10 y to 3 month spread
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
lst.sym <- c("GDP.Value", "GDPI.Value")
if ( require_columns(df.data, lst.sym ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "GPDI__by__GDP"
  df.data[[str.symbol.new]] <- 
    ( df.data[[lst.sym[[1]]]] / df.data[[lst.sym[[2]]]] )
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

# Clean up memory
rm(lst.sym)


#--------------------------------------------------------------------------
# Calculate returns
#--------------------------------------------------------------------------

# returns for base case (S&P 500)
lst.syms <- c("X_GSPC.GSPC.Close")

if ( require_columns( df.data, lst.syms ) ){

  # Define the new symbol and make the calculation
  str.symbol.new <- "retBase"
  df.data[[str.symbol.new]] <- ROC(df.data[[lst.syms[[1]]]])
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

# Tidy up
rm(lst.syms)

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
