



# This became onerous to keep in the main mark-up document so I broke it out.

string.symbol = c("CPIAUCSL", "USREC")
string.source = c("FRED", "FRED")
string.description = c(
  "Consumer Price Index for All\nUrban Consumers: All Items",
  "NBER based Recession Indicators"
)
string.label.y = c("Index 1982-1984=100", "+1 or 0")
float.expense.ratio = c(-1.00, -1.00)       
date.series.start = as.Date("1900-01-01")
date.series.end = as.Date("1900-01-01")
df.symbols = data.frame(
  string.symbol,
  string.source,
  string.description,
  string.label.y,
  float.expense.ratio,
  date.series.start,
  date.series.end,
  stringsAsFactors = FALSE
)

df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "UNRATE",
      string.source = "FRED",
      string.description = "Civilian Unemployment Rate U-3",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start = as.Date("1900-01-01"),
      date.series.end = as.Date("1900-01-01")
    )
  )

