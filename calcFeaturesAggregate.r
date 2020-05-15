

# Loans + reserves to compare to deposits
df.data$TOTLNNSA.PLUS.WRESBAL <- df.data$TOTLNNSA + df.data$WRESBAL
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTLNNSA.PLUS.WRESBAL",
      string.source = "Calc",
      string.description = "Total Loans Plus All Reserves (TOTLNNSA + WRESBAL)",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date(max(
        c(df.symbols$date.series.start[df.symbols$string.symbol == 'TOTLNNSA'], index(WRESBAL[1]))
      )) ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$string.symbol == 'TOTLNNSA'], index(tail(WRESBAL, 1))
      )))

    )
  )

# Loans + reserves to compare to deposits
df.data$TOTLLNSA.PLUS.WRESBAL <- df.data$TOTLLNSA + df.data$WRESBAL
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTLLNSA.PLUS.WRESBAL",
      string.source = "Calc",
      string.description = "Total Loans Plus All Reserves (TOTLNNSA + WRESBAL)",
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

# S&P 500 Open divided by the 200 day SMA
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

# S&P 200 day SMA minus 50 day SMA
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

# Trading signal based on S&P 50 day SMA minus 200 day SMA
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

# Mean value of the P/E series, excluding highs that occured during the great recession
df.data$MULTPLSP500PERATIOMONTH_Mean <-
  mean(df.data$MULTPLSP500PERATIOMONTH[as.Date(index(df.data)) > as.Date("1990-01-01") &
                                         df.data$MULTPLSP500PERATIOMONTH < 50])
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "MULTPLSP500PERATIOMONTH_Mean",
      string.source = "Calc",
      string.description = "S&P 500 TTM P/E Average \n (Excludes Values Greater Than 50)",
      string.label.y = "Index",
      float.expense.ratio = -1.00,
      date.series.start = as.Date(df.symbols$date.series.start[df.symbols$string.symbol == 'MULTPLSP500PERATIOMONTH']) ,
      date.series.end = as.Date(df.symbols$date.series.end[df.symbols$string.symbol == 'MULTPLSP500PERATIOMONTH'])
    )
  )




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
    #print(dfTestPoint)
    dfMax = dfTestPoint[dfTestPoint[, 1] > dCurrent[1],]
    #print(dfMax)
    
    # If the test point is valid
    df.symbols[df.symbols$string.symbol == strName, strColMaxName] <-
      FALSE
    if (!is.na(dfTestPoint) && is.numeric(dfMax)) {
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
  
  iDays <- 30
  df.symbols <- getRecentHigh(df.data, df.symbols, iDays, strName)
  
  iDays <- 180
  df.symbols <- getRecentHigh(df.data, df.symbols, iDays, strName)
}

# This speeds some of the upcoming operations
df.data$date = as.Date(rownames(df.data))
