

#' Interpolate all series
#' 
#' This snippet of code gets used by both the jupyter sheets and the rmd files
#' to interpolate and tidy the data. At some point it needs to be put into a function
#'
#' @param df.symbols Data frame with symbols. Available series must exist as zoo objects in the global environment.
#'
#' @return df.data Data frame with all series interpolated or carried forward to daily frequency
#' @export
#'
#' @examples
calcInterpolate <- function(df.symbols) {

  available <- !is.na(df.symbols$string.symbol_safe) &
    nzchar(df.symbols$string.symbol_safe) &
    vapply(df.symbols$string.symbol_safe, exists, logical(1), inherits = TRUE)
  if ("status" %in% names(df.symbols)) {
    available <- available & !is.na(df.symbols$status) & df.symbols$status == "ok"
  }

  available_symbols <- df.symbols$string.symbol_safe[available]
  if (!length(available_symbols)) {
    stop("No successfully fetched symbol data is available to interpolate.")
  }

  xtsData <- get(available_symbols[1])
  for (symbol in available_symbols[-1]) {
    xtsData <- merge(xtsData, get(symbol))
    #print(df.symbols$string.symbol[idx])
  }
  
  df.data <- data.frame(xtsData)
  
  # These two lines evenly space all the data at a daily interval
  zooData <- zoo(df.data, as.Date(rownames(df.data)))
  dtStartDf <- start(zooData)
  dt.end.predictionDf <- end(zooData)
  zooData <-
    merge(zooData, zoo(, seq(dtStartDf, dt.end.predictionDf, by = 1)), all =
            TRUE)
  df.data <- data.frame(zooData)
  
  # The recession data is binary and needs to be carried forward
  if( ("string.symbol" %in% names(df.symbols)) ){
    
    # The column exists, does it contain the US recession series?
    if( any(df.symbols$string.symbol == "USREC", na.rm = TRUE) ){
      df.data$USREC <- na.locf(df.data$USREC)
    }
  }
  
  # This sections removes NA's with an approximation. It returns a zoo object so the
  # row names have to be reset
  rowTemp <- rownames(df.data)
  df.data <- data.frame(na.approx(df.data, rule = 2))
  rownames(df.data) <- rowTemp
  
  return(df.data)
  
}

