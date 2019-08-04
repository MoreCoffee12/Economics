

# The FRED recession data is 1 for the months that
# are in recession and 0 for months not in recession
# so we will use the diff command to save off the
# indexes where the value changes from 0 to 1 or
# 1 to 0. I found this idea in a stack overflow
# article:
# http://stackoverflow.com/questions/21739012/r-recession-dates-conversion
# I found it was more robust than the nberShade()
# command for the xts time series data.
dtStart <- df.data$date[which(diff(df.data$USREC) == 1) + 1]
dt.end.prediction   <- df.data$date[which(diff(df.data$USREC) == -1)]
dtInitStart <- as.Date(as.yearmon(dtStart) - 12 / 12)
dtInitEnd <- as.Date(as.yearmon(dtStart) - 1 / 12)

# We need to cast the recession data into
# a dataframe.
dfRecession <-
  data.frame(
    initStart = dtInitStart,
    initEnd = dtInitEnd,
    start = dtStart,
    end = dt.end.prediction[-1]
  )
dfRecession <- subset(dfRecession, dtStart >= min(df.data$date))

# Add the recession initiation date as a time series
df.data$RecInit <- rep(0, nrow(df.data))
df.data$RecInit_Smooth <- rep(0, nrow(df.data))

for (idx in 1:nrow(dfRecession)) {
  df.data$RecInit[which(df.data$date > dfRecession$initStart[idx] &
                          df.data$date < dfRecession$initEnd[idx])] = 1
  df.data$RecInit_Smooth[which(df.data$date > dfRecession$initStart[idx] &
                                 df.data$date < dfRecession$initEnd[idx])] =
    cumsum(df.data$RecInit[which(df.data$date > dfRecession$initStart[idx] &
                                   df.data$date < dfRecession$initEnd[idx])])
}

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RecInit",
      string.source = "Calc",
      string.description = "1 for Recession Initiation Period, 0 For All Else",
      string.label.y = "(-)",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(USREC[1])),
      date.series.end = as.Date(index(tail(USREC, 1)))
    )
  )

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RecInit_Smooth",
      string.source = "Calc",
      string.description = "1 for Recession Initiation Period, 0 For All Else (Smoothed)",
      string.label.y = "(-)",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start = as.Date(index(USREC[1])),
      date.series.end = as.Date(index(tail(USREC, 1)))
    )
  )


# Additional smoothing and processing for the smooth curve
df.data$RecInit_Smooth <- sgolayfilt(df.data$RecInit_Smooth, p=3, n=201, m=0, ts=1)
df.data$RecInit_Smooth[df.data$RecInit_Smooth < 0] = 0
#df.data$RecInit_Smooth[df.data$RecInit_Smooth > 1] = 1
#df.data$RecInit_Smooth[diff(df.data$RecInit_Smooth)<0] = 1
#df.data$RecInit_Smooth = df.data$RecInit_Smooth + df.data$USREC
#df.data$RecInit_Smooth[df.data$RecInit_Smooth>1] = 1
df.data$RecInit_Smooth <- df.data$RecInit_Smooth / max(df.data$RecInit_Smooth)
df.data$RecInit_Smooth <- jitter(df.data$RecInit_Smooth, amount = 0.01)

