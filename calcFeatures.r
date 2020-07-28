

#' Calculate data features
#'
#' @param df.data Data frame with the data
#' @param df.symbols Dataframe with symbol meta data
#'
#' @return
#' @export
#'
#' @examples
calcFeatures <- function(df.data, df.symbols){

  # Loop through each of the columns
  for (str.symbol in names(df.data)) {
    
    # Define series description, dates and deal with root series symbols.

    # This section looks up the description. The "." extension on trading data series
    # from Yahoo requires some additional look up.
    str.description <-
      df.symbols[grep(paste("^", str.symbol, "$", sep = ""), df.symbols$string.symbol), ]$string.description
    str.symbol.root <- str.symbol
    
    # Descriptions must be associated with the root data series name. If the
    # symbol table is empty and there is a "." in the name, look up the root
    # symbol and use that.
    if (length(str.description) <= 0) {
      str.suffix <- ""
      if (grepl("\\.", str.symbol)) {
        str.symbol.root <- substr(str.symbol, 1, regexpr("\\.", str.symbol) - 1)
        str.suffix <-
          substr(str.symbol, regexpr("\\.", str.symbol) + 1, nchar(str.symbol))
        str.suffix <- paste(" (", str.suffix, ")", sep = "")
      }
    }
    # The new series will have the same start date as the original series
    # root symbol
    date.temp <-
      df.symbols[grep(paste("^", str.symbol.root, "$", sep = ""), df.symbols$string.symbol), ]$date.series.start
    date.temp.end <-
      df.symbols[grep(paste("^", str.symbol.root, "$", sep = ""), df.symbols$string.symbol), ]$date.series.end
    
    
    # Start with the YoY calculation
    str.symbolYoY <- paste(str.symbol, "_YoY", sep = "")
    df.data[str.symbolYoY] <- CalcYoY(df.data, str.symbol, 365)
    print(paste(str.symbol,'-',str.symbolYoY, '-', str.description))
    df.symbols <-
      rbind(
        df.symbols,
        data.frame(
          string.symbol = str.symbolYoY,
          string.source = "Calc",
          string.description = paste(str.description, "\nYear over Year", sep =
                                       ""),
          string.label.y = "Percent",
          float.expense.ratio = -1.00,
          date.series.start = date.temp,
          date.series.end = date.temp.end
        )
      )
    
    
    # These series were added to help evaluate structured products and
    # answer the question, what is the probability of a decline over a period of
    # 4 and 5 years.
    str.symbolYoY4 <- paste(str.symbol, "_YoY4", sep = "")
    df.data[str.symbolYoY4] <- CalcYoY(df.data, str.symbol, (365*4))
    #print(paste(str.symbol,'-',str.symbolYoY4, '-', str.description))
    df.symbols <-
      rbind(
        df.symbols,
        data.frame(
          string.symbol = str.symbolYoY4,
          string.source = "Calc",
          string.description = paste(str.description, "\n4 Year over 4 Year", sep =
                                       ""),
          string.label.y = "Percent",
          float.expense.ratio = -1.00,
          date.series.start = date.temp,
          date.series.end = date.temp.end
        )
      )
    
    # The 5-year series
    str.symbolYoY5 <- paste(str.symbol, "_YoY5", sep = "")
    df.data[str.symbolYoY5] <- CalcYoY(df.data, str.symbol, (365*5))
    #print(paste(str.symbol,'-',str.symbolYoY5, '-', str.description))
    df.symbols <-
      rbind(
        df.symbols,
        data.frame(
          string.symbol = str.symbolYoY5,
          string.source = "Calc",
          string.description = paste(str.description, "\n5 Year over 5 Year", sep =
                                       ""),
          string.label.y = "Percent",
          float.expense.ratio = -1.00,
          date.series.start = date.temp,
          date.series.end = date.temp.end
        )
      )
    
    # Smooth the series, kernel of 1-year
    strNewYLabel <-
      df.symbols[grep(paste("^", str.symbol.root, "$", sep = ""), df.symbols$string.symbol), ]$tring.label.y 
    str.symbolSmooth <- paste(str.symbol, "_Smooth", sep = "")
    df.data[str.symbolSmooth] <-
      sgolayfilt(
        df.data[, str.symbol],
        p = 3,
        n = 365,
        m = 0,
        ts = 1
      )
    df.symbols <-
      rbind(
        df.symbols,
        data.frame(
          string.symbol = str.symbolSmooth,
          string.source = "Calc",
          string.description = paste(
            "Savitsky-Golay Smoothed (p=3, n=365)\n",
            str.description,
            sep = ""
          ),
          string.label.y = paste(strNewYLabel, "/period", sep =
                           ""),
          float.expense.ratio = -1.00,
          date.series.start = date.temp,
          date.series.end = date.temp.end
        )
      )
    
    # Smooth the series, kernel of 15 days (inspired by RSI length)
    strNewYLabel <-
      df.symbols[grep(paste("^", str.symbol.root, "$", sep = ""), df.symbols$string.symbol), ]$tring.label.y 
    str.symbolSmooth <- paste(str.symbol, "_Smooth.short", sep = "")
    df.data[str.symbolSmooth] <-
      sgolayfilt(
        df.data[, str.symbol],
        p = 3,
        n = 15,
        m = 0,
        ts = 1
      )
    df.symbols <-
      rbind(
        df.symbols,
        data.frame(
          string.symbol = str.symbolSmooth,
          string.source = "Calc",
          string.description = paste(
            "Savitsky-Golay Smoothed (p=3, n=15)\n",
            str.description,
            sep = ""
          ),
          string.label.y = paste(strNewYLabel, "/period", sep =
                           ""),
          float.expense.ratio = -1.00,
          date.series.start = date.temp,
          date.series.end = date.temp.end
        )
      )
    
    # Smooth and derivative in one step
    strNewYLabel <-
      df.symbols[grep(paste("^", str.symbol.root, "$", sep = ""), df.symbols$string.symbol), ]$tring.label.y 
    str.symbolSmoothDer <- paste(str.symbol, "_SmoothDer", sep = "")
    df.data[str.symbolSmoothDer] <-
      sgolayfilt(
        df.data[, str.symbol],
        p = 3,
        n = 501,
        m = 1,
        ts = 1
      )
    df.symbols <-
      rbind(
        df.symbols,
        data.frame(
          string.symbol = str.symbolSmoothDer,
          string.source = "Calc",
          string.description = paste("Derivative of Smoothed\n", str.description, sep =
                         ""),
          string.label.y = paste(strNewYLabel, "/period", sep =
                           ""),
          float.expense.ratio = -1.00,
          date.series.start = date.temp,
          date.series.end = date.temp.end
        )
      )
    
    # Take the log
    # print(str.symbol)
    strNewYLabel <-
      df.symbols[grep(paste("^", str.symbol.root, "$", sep = ""), df.symbols$string.symbol), ]$tring.label.y 
    str.symbolLog <- paste(str.symbol, "_Log", sep = "")
    if (any(df.data[,str.symbol] <=0)){
      df.data[str.symbolLog] <- 0*(df.data[, str.symbol])
      print(paste(str.symbol," has zero or negative values. Log series will be zero.", sep=""))
    }else{
      df.data[str.symbolLog] <- log(df.data[, str.symbol])
    }
    df.data[!is.finite(df.data[, str.symbolLog]), str.symbolLog] <- NA
    df.data[, str.symbolLog] <- na.approx(df.data[, str.symbolLog], rule = 2)
    df.symbols <-
      rbind(
        df.symbols,
        data.frame(
          string.symbol = str.symbolLog,
          string.source = "Calc",
          string.description = paste("Log of ", str.description, sep =
                         ""),
          string.label.y = paste("log(", strNewYLabel, ")", sep =
                           ""),
          float.expense.ratio = -1.00,
          date.series.start = date.temp,
          date.series.end = date.temp.end
        )
      )
    
    
    # Add the 200 day moving average
    mav <- function(x, n = 200) {
      stats::filter(x, rep(1 / n, n), sides = 1)
    }
    strNewYLabel <-
      df.symbols[grep(paste("^", str.symbol.root, "$", sep = ""), df.symbols$string.symbol), ]$tring.label.y 
    str.symbolMVA200 <- paste(str.symbol, "_mva200", sep = "")
    df.data[str.symbolMVA200] <- mav(df.data[, str.symbol])
    df.data[, str.symbolMVA200] <-
      na.approx(df.data[, str.symbolMVA200], rule = 2)
    df.symbols <-
      rbind(
        df.symbols,
        data.frame(
          string.symbol = str.symbolMVA200,
          string.source = "Calc",
          string.description = paste(str.description, " 200 Day MA", sep =
                         ""),
          string.label.y = paste(strNewYLabel, " 200 Day MA", sep =
                           ""),
          float.expense.ratio = -1.00,
          date.series.start = date.temp,
          date.series.end = date.temp.end
        )
      )
    
    # Add the 50 day moving average
    strNewYLabel <-
      df.symbols[grep(paste("^", str.symbol.root, "$", sep = ""), df.symbols$string.symbol), ]$tring.label.y 
    str.symbolMVA050 <- paste(str.symbol, "_mva050", sep = "")
    df.data[str.symbolMVA050] <- mav(df.data[, str.symbol], n = 50)
    df.data[, str.symbolMVA050] <-
      na.approx(df.data[, str.symbolMVA050], rule = 2)
    df.symbols <-
      rbind(
        df.symbols,
        data.frame(
          string.symbol = str.symbolMVA050,
          string.source = "Calc",
          string.description = paste(str.description, " 50 Day MA", sep =
                         ""),
          string.label.y = paste(strNewYLabel, " 50 Day MA", sep =
                           ""),
          float.expense.ratio = -1.00,
          date.series.start = date.temp,
          date.series.end = date.temp.end
        )
      )
    
  }  
  
  return(list(df.data, df.symbols))
    
}

