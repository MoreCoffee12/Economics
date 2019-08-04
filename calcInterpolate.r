

#' Interpolate all series
#' 
#' This snippet of code gets used by both the jupyter sheets and the rmd files
#' to interpolate and tidy the data. At some point it needs to be put into a function
#'
#' @param df.symbols Data frame with symbols. Each of these must exist as a zoo object in the global environment.
#'
#' @return df.data Dataframe with all series interpolated or carried forward to daily frequency
#' @export
#'
#' @examples
calcInterpolate <- function(df.data, df.symbols) {

  xtsData <- get(df.symbols$string.symbol[1])
  for (idx in 2:nrow(df.symbols)) {
    xtsData <- merge(xtsData,  get(df.symbols$string.symbol[idx]))
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
  df.data$USREC <- na.locf(df.data$USREC)
  
  # This sections removes NA's with an approximation. It returns a zoo object so the
  # row names have to be reset
  rowTemp <- rownames(df.data)
  df.data <- data.frame(na.approx(df.data, rule = 2))
  rownames(df.data) <- rowTemp
  
  return(df.data)
  
}

