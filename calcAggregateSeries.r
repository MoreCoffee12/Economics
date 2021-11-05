



# Create an aggregate for retail sales
df.data$RSALESAGG <-
  rowMeans(df.data[, c("RRSFS", "RSALES")], na.rm = TRUE)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RSALESAGG",
      string.source = "Calc",
      string.description = "Real Retail and Food Services Sales\n(RRSFS and RSALES)",
      string.label.y = "Millions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(index(RSALES[1])) ,
      date.series.end = as.Date(index(tail(RRSFS, 1)))
    )
  )

# Difference between monthly SA and NSA series
df.data$BUSLOANS.minus.BUSLOANSNSA <-
  (df.data$BUSLOANS - df.data$BUSLOANSNSA)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BUSLOANS.minus.BUSLOANSNSA",
      string.source = "Calc",
      string.description = "Business Loans (Montlhy) SA - NSA",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(BUSLOANS[1]), index(BUSLOANSNSA[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(BUSLOANS, 1)), index(tail(BUSLOANSNSA, 1))
      )))
    )
  )

# Difference between monthly SA and NSA series / GDP
df.data$BUSLOANS.minus.BUSLOANSNSA.by.GDP <-
  (df.data$BUSLOANS.minus.BUSLOANSNSA / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BUSLOANS.minus.BUSLOANSNSA.by.GDP",
      string.source = "Calc",
      string.description = "Business Loans (Montlhy) SA - NSA divided by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(BUSLOANS[1]), index(GDP[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(BUSLOANS, 1)), index(tail(GDP, 1))
      )))
    )
  )


# Normalize business loans (monthly, SA) by GDP
df.data$BUSLOANS.by.GDP <- (df.data$BUSLOANS / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BUSLOANS.by.GDP",
      string.source = "Calc",
      string.description = "Business Loans Normalized by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(BUSLOANS[1]), index(GDP[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(BUSLOANS, 1)), index(tail(GDP, 1))
      )))
    )
  )


# Business loans (monthly, SA) interest
df.data$BUSLOANS.INTEREST <-
  (df.data$BUSLOANS * df.data$DGS10) / 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BUSLOANS.INTEREST",
      string.source = "Calc",
      string.description = "Business Loans (Monthly, SA)\nAdjusted Interest Burdens",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'BUSLOANS'], index(DGS10[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'BUSLOANS'], index(tail(DGS10, 1))
      )))
    )
  )

# Business loans interest divided by GDP
df.data$BUSLOANS.INTEREST.by.GDP <-
  (df.data$BUSLOANS.INTEREST / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BUSLOANS.INTEREST.by.GDP",
      string.source = "Calc",
      string.description = "Business Loans (Monthly, SA)\nAdjusted Interest Burden Divided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'BUSLOANS.INTEREST'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'BUSLOANS.INTEREST'], index(tail(GDP, 1))
      )))
    )
  )


# Normalize business loans (montlhy, NSA) by GDP
df.data$BUSLOANSNSA.by.GDP <-
  (df.data$BUSLOANSNSA / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BUSLOANSNSA.by.GDP",
      string.source = "Calc",
      string.description = "Business Loans Normalized by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(BUSLOANSNSA[1]), index(GDP[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(BUSLOANSNSA, 1)), index(tail(GDP, 1))
      )))
    )
  )

# Normalize business loans (weekly, SA) by GDP
df.data$TOTCI.by.GDP <- (df.data$TOTCI / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTCI.by.GDP",
      string.source = "Calc",
      string.description = "Business Loans (Weekly, SA) Normalized by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(TOTCI[1]), index(GDP[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(TOTCI, 1)), index(tail(GDP, 1))
      )))
    )
  )

# Normalize (weekly, NSA) business loans by GDP
df.data$TOTCINSA.by.GDP <- (df.data$TOTCINSA / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTCINSA.by.GDP",
      string.source = "Calc",
      string.description = "Business Loans (Weekly, NSA) Normalized by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(TOTCINSA[1]), index(GDP[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(TOTCINSA, 1)), index(tail(GDP, 1))
      )))
    )
  )

# Business loans (weekly, NSA) interest
df.data$TOTCINSA.INTEREST <-
  (df.data$TOTCINSA * df.data$DGS10) / 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTCINSA.INTEREST",
      string.source = "Calc",
      string.description = "Business Loans (Weekly, NSA)\nAdjusted Interest Burdens",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'TOTCINSA'], index(DGS10[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'TOTCINSA'], index(tail(DGS10, 1))
      )))
    )
  )

# Business loans (weekly, NSA) interest divided by GDP
df.data$TOTCINSA.INTEREST.by.GDP <-
  (df.data$TOTCINSA.INTEREST / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTCINSA.INTEREST.by.GDP",
      string.source = "Calc",
      string.description = "Business Loans (weekly, NSA)\nAdjusted Interest Burden Divided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'TOTCINSA.INTEREST.by.GDP'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'TOTCINSA.INTEREST.by.GDP'], index(tail(GDP, 1))
      )))
    )
  )

# Normalize real personal income by GDP
df.data$W875RX1.by.GDP <- (df.data$W875RX1 / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "W875RX1.by.GDP",
      string.source = "Calc",
      string.description = "Real Personal Income Normalized by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(W875RX1[1]), index(GDP[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(W875RX1, 1)), index(tail(GDP, 1))
      )))
    )
  )

# Normalize NSA personal income by GDP
df.data$A065RC1A027NBEA.by.GDP <-
  (df.data$A065RC1A027NBEA / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "A065RC1A027NBEA.by.GDP",
      string.source = "Calc",
      string.description = "Personal Income (NSA) Normalized by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(A065RC1A027NBEA[1]), index(GDP[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(A065RC1A027NBEA, 1)), index(tail(GDP, 1))
      )))
    )
  )

# Normalize SA personal income by GDP
df.data$PI.by.GDP <- (df.data$PI / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "PI.by.GDP",
      string.source = "Calc",
      string.description = "Personal Income (SA) Normalized by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(PI[1]), index(GDP[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(PI, 1)), index(tail(GDP, 1))
      )))
    )
  )

# Normalize pre-tax corporate profits by GDP
df.data$A053RC1Q027SBEA.by.GDP <-
  (df.data$A053RC1Q027SBEA / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "A053RC1Q027SBEA.by.GDP",
      string.source = "Calc",
      string.description = "National income: Corporate profits\nbefore tax (without IVA and CCAdj)\nNormalized by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(A053RC1Q027SBEA[1]), index(GDP[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(A053RC1Q027SBEA, 1)), index(tail(GDP, 1))
      )))
    )
  )

# Normalize pre-tax corporate profits (with inventory and capital adjustments) by GDP
df.data$CPROFIT.by.GDP <- (df.data$CPROFIT / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CPROFIT.by.GDP",
      string.source = "Calc",
      string.description = "National income: Corporate profits\nbefore tax (with IVA and CCAdj)\nNormalized by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'CPROFIT'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'CPROFIT'], index(tail(GDP, 1))
      )))
    )
  )


# Consumer loans as a percent of GDP
df.data$CONSUMERNSA.by.GDP <-
  (df.data$CONSUMERNSA / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CONSUMERNSA.by.GDP",
      string.source = "Calc",
      string.description = "Consumer Loans Not Seasonally\nAdjusted divided by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'CONSUMERNSA'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'CONSUMERNSA'], index(tail(GDP, 1))
      )))
    )
  )

# Residential real estate (monthly, NSA) loans as a percent of GDP
df.data$RREACBM027NBOG.by.GDP <-
  (df.data$RREACBM027NBOG / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RREACBM027NBOG.by.GDP",
      string.source = "Calc",
      string.description = "Residental Real Estate Loans (Monthly, NSA)\ndivided by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'RREACBM027NBOG'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'RREACBM027NBOG'], index(tail(GDP, 1))
      )))
    )
  )

# Residential real estate (monthly, SA) loans as a percent of GDP
df.data$RREACBM027SBOG.by.GDP <-
  (df.data$RREACBM027SBOG / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RREACBM027SBOG.by.GDP",
      string.source = "Calc",
      string.description = "Residental Real Estate Loans (Monthly, SA)\ndivided by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'RREACBM027SBOG'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'RREACBM027SBOG'], index(tail(GDP, 1))
      )))
    )
  )

# Residential real estate (weekly, SA) loans as a percent of GDP
df.data$RREACBW027SBOG.by.GDP <-
  (df.data$RREACBW027SBOG / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RREACBW027SBOG.by.GDP",
      string.source = "Calc",
      string.description = "Residental Real Estate Loans (Weekly, SA)\ndivided by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'RREACBW027SBOG'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'RREACBW027SBOG'], index(tail(GDP, 1))
      )))
    )
  )

# Residential real estate (weekly, NSA) loans as a percent of GDP
df.data$RREACBW027NBOG.by.GDP <-
  (df.data$RREACBW027NBOG / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "RREACBW027NBOG.by.GDP",
      string.source = "Calc",
      string.description = "Residental Real Estate Loans (Weekly, NSA)\ndivided by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'RREACBW027NBOG'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'RREACBW027NBOG'], index(tail(GDP, 1))
      )))
    )
  )

# All durable goods as percet of GDP. Have to convert from millions to
# billions to be consistent with H.8 and GDP series
df.data$UMDMNO <- df.data$UMDMNO / 1000
df.data$UMDMNO.by.GDP <-
  (df.data$UMDMNO / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "UMDMNO.by.GDP",
      string.source = "Calc",
      string.description = "Durable Goods (Monthly, NSA)\ndivided by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'UMDMNO'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'UMDMNO'], index(tail(GDP, 1))
      )))
    )
  )

# All durable goods as percet of GDP. Have to convert from millions to
# billions to be consistent with H.8 and GDP series
df.data$DGORDER <- df.data$DGORDER / 1000
df.data$DGORDER.by.GDP <-
  (df.data$DGORDER / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DGORDER.by.GDP",
      string.source = "Calc",
      string.description = "Durable Goods (Monthly, NSA)\ndivided by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'UMDMNO'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'UMDMNO'], index(tail(GDP, 1))
      )))
    )
  )

# All home mortgages as percet of GDP. Have to convert from millions to
# billions to be consistent with H.8 and GDP series
df.data$ASHMA <- df.data$ASHMA / 1000
df.data$ASHMA.by.GDP <-
  (df.data$ASHMA / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASHMA.by.GDP",
      string.source = "Calc",
      string.description = "Home Mortgages (Quarterly, NSA)\ndivided by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'ASHMA'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'ASHMA'], index(tail(GDP, 1))
      )))
    )
  )


# All home mortgages interest burden
df.data$ASHMA.INTEREST <-
  (df.data$ASHMA * df.data$MORTGAGE30US) / 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASHMA.INTEREST",
      string.source = "Calc",
      string.description = "Home Mortgages (Quarterly, NSA)\n 30-Year Fixed Interest Burdens",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'ASHMA'], index(MORTGAGE30US[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'ASHMA'], index(tail(MORTGAGE30US, 1))
      )))
    )
  )

# All home mortgages interest burden divided by GDP
df.data$ASHMA.INTEREST.by.GDP <-
  (df.data$ASHMA.INTEREST / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "317491-948457.by.GDP",
      string.source = "Calc",
      string.description = "Home Mortgages (Quarterly, NSA)\n 30-Year Fixed Interest Burdens\nDivided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'ASHMA.INTEREST'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'ASHMA.INTEREST'], index(tail(GDP, 1))
      )))
    )
  )

# Consumer loans interest using the 24 month consumer series.
df.data$CONSUMERNSA.INTEREST <-
  (df.data$CONSUMERNSA * df.data$TERMCBPER24NS) / 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CONSUMERNSA.INTEREST",
      string.source = "Calc",
      string.description = "Consumer Loans (Not Seasonally\nAdjusted) Interest Burdens",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'TERMCBPER24NS'], index(DGS10[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'TERMCBPER24NS'], index(tail(DGS10, 1))
      )))
    )
  )

# Total loans interest divided by GDP
df.data$CONSUMERNSA.INTEREST.by.GDP <-
  (df.data$CONSUMERNSA.INTEREST / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CONSUMERNSA.INTEREST.by.GDP",
      string.source = "Calc",
      string.description = "Consumer Loans (Not Seasonally\nAdjusted) Interest Burden Divided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'CONSUMERNSA.INTEREST'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'CONSUMERNSA.INTEREST'], index(tail(GDP, 1))
      )))
    )
  )


# Create aggregate of total loans
df.data$TOTLNNSA <-
  (df.data$BUSLOANS + df.data$REALLNNSA + df.data$CONSUMERNSA)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTLNNSA",
      string.source = "Calc",
      string.description = "Total Loans Not Seasonally\nAdjusted (BUSLOANS+REALLNSA+CONSUMERNSA)",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(REALLNNSA[1]), index(CONSUMERNSA[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(REALLNNSA, 1)), index(tail(CONSUMERNSA, 1))
      )))
    )
  )

# Total loans as a percent of GDP
df.data$TOTLNNSA.by.GDP <-
  (df.data$TOTLNNSA / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTLNNSA.by.GDP",
      string.source = "Calc",
      string.description = "Total Loans Not Seasonally\nAdjusted divided by GDP",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'TOTLNNSA'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'TOTLNNSA'], index(tail(GDP, 1))
      )))
    )
  )

# Total loans interest
df.data$TOTLNNSA.INTEREST <-
  (df.data$TOTLNNSA * df.data$DGS10) / 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTLNNSA.INTEREST",
      string.source = "Calc",
      string.description = "Total Loans Not Seasonally\nAdjusted Interest Burdens",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'TOTLNNSA'], index(DGS10[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'TOTLNNSA'], index(tail(DGS10, 1))
      )))
    )
  )

# Total loans interest divided by GDP
df.data$TOTLNNSA.INTEREST.by.GDP <-
  (df.data$TOTLNNSA.INTEREST / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "TOTLNNSA.INTEREST.by.GDP",
      string.source = "Calc",
      string.description = "Total Loans Not Seasonally\nAdjusted Interest Burden Divided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'TOTLNNSA.INTEREST'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'TOTLNNSA.INTEREST'], index(tail(GDP, 1))
      )))
    )
  )



# Reserve balances (in billions) divided by GDP
df.data$WRESBAL.by.GDP <-
  (df.data$WRESBAL / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "WRESBAL.by.GDP",
      string.source = "Calc",
      string.description = "Reserve Balances with Federal\nReserve Banks Divided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'WRESBAL'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'WRESBAL'], index(tail(GDP, 1))
      )))
    )
  )

# Don't know why, but not all reserve money data series are in billions, some are in millions. Fix this.
df.data$EXCSRESNW <- df.data$EXCSRESNW / 1000

# Excess reserve balances (in billions) divided by GDP
df.data$EXCSRESNW.by.GDP <-
  (df.data$EXCSRESNW / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "EXCSRESNW.by.GDP",
      string.source = "Calc",
      string.description = "Excess Reserves of Depository Institutions\nDivided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'WRESBAL'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'WRESBAL'], index(tail(GDP, 1))
      )))
    )
  )

# Federal reserve repos, from millions to billions
df.data$WLRRAL <- df.data$WLRRAL / 1000

# Reverse repos (in billions) divided by GDP
df.data$WLRRAL.by.GDP <-
  (df.data$WLRRAL / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "WLRRAL.by.GDP",
      string.source = "Calc",
      string.description = "Liabilities and Capital:\nLiabilities: Reverse Repurchase Agreements:\nWednesday Level (NSA)\nDivided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'WLRRAL'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'WLRRAL'], index(tail(GDP, 1))
      )))
    )
  )

# Secured overnight variation
df.data$SOFR99.minus.SOFR1 <-
  (df.data$SOFR99 - df.data$SOFR1)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SOFR99.minus.SOFR1",
      string.source = "Calc",
      string.description = "Secured Overnight Financing Rate:\n99th Percentile - 1st Percentile",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'SOFR99'], index(SOFR1[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'SOFR99'], index(tail(SOFR1, 1))
      )))
    )
  )


# Census data in millions, needs to be billions.Already fixed in the descriptions
df.data$IMPCH <- df.data$IMPCH / 1000
df.data$EXPCH <- df.data$EXPCH / 1000

# China trade balance
df.data$EXPCH.minus.IMPCH <-
  (df.data$EXPCH - df.data$IMPCH)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "EXPCH.minus.IMPCH",
      string.source = "Calc",
      string.description = "U.S. Exports to China (FAS Basis) -\nU.S. Imports to China (Customs Basis)",
      string.label.y = "Billions of dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'EXPCH'], index(IMPCH[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'EXPCH'], index(tail(IMPCH, 1))
      )))
    )
  )

df.data$EXPMX <- df.data$EXPMX / 1000
df.data$IMPMX <- df.data$IMPMX / 1000

# Mexico trade balance
df.data$EXPMX.minus.IMPMX <-
  (df.data$EXPMX - df.data$IMPMX)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "EXPMX.minus.IMPCH",
      string.source = "Calc",
      string.description = "U.S. Exports to Mexico (FAS Basis) -\nU.S. Imports to Mexico (Customs Basis)",
      string.label.y = "Billions of dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'EXPMX'], index(IMPMX[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'EXPMX'], index(tail(IMPMX, 1))
      )))
    )
  )


# Z.1 in millions, needs to be in billions. Already fixed in the descriptions
df.data$ASTLL <- df.data$ASTLL / 1000
df.data$FBDILNECA <- df.data$FBDILNECA / 1000
df.data$ASOLAL <- df.data$ASOLAL / 1000
df.data$ASTMA <- df.data$ASTMA / 1000
df.data$ASMRMA <- df.data$ASMRMA / 1000
df.data$ASCMA <- df.data$ASCMA / 1000

# Nonfinancial corporate business; security repurchase agreements; asset, Level divided by GDP
df.data$SRPSABSNNCB.by.GDP <-
  (df.data$SRPSABSNNCB / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "SRPSABSNNCB.by.GDP",
      string.source = "Calc",
      string.description = "Nonfinancial corporate business;\nsecurity repurchase agreements;\nasset, Level (NSA)\nDivided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'SRPSABSNNCB'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'SRPSABSNNCB'], index(tail(GDP, 1))
      )))
    )
  )




# Total loans divided by GDP
df.data$ASTLL.by.GDP <-
  (df.data$ASTLL / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASTLL.by.GDP",
      string.source = "Calc",
      string.description = "All sectors; total loans;\nliability, Level (NSA)\nDivided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'ASTLL'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'ASTLL'], index(tail(GDP, 1))
      )))
    )
  )


# Z.1 in millions, needs to be in billions. Already fixed in the descriptions
df.data$ASFMA <- df.data$ASFMA / 1000

# Farm loans divided by GDP
df.data$ASFMA.by.GDP <-
  (df.data$ASFMA / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASFMA.by.GDP",
      string.source = "Calc",
      string.description = "All sectors; farm\nmortgages; asset, Level (NSA)\nDivided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'ASFMA'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'ASFMA'], index(tail(GDP, 1))
      )))
    )
  )

# Take a look at farm mortgages as a percentage of total loans.
df.data$ASFMA.by.ASTLL <- (df.data$ASFMA / df.data$ASTLL) * 100 
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASFMA.by.ASTLL",
      string.source = "Calc",
      string.description = "All sectors; total loans\nDivided by farm mortgages",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'ASFMA'], index(ASTLL[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'ASFMA'], index(tail(ASTLL, 1))
      )))
    )
  )

# Farm mortgages interest burden
df.data$ASFMA.INTEREST <-
  (df.data$ASFMA * df.data$MORTGAGE30US) / 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASFMA.INTEREST",
      string.source = "Calc",
      string.description = "Farm Mortgages (Quarterly, NSA)\n 30-Year Fixed Interest Burdens",
      string.label.y = "Billions of U.S. Dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'ASFMA'], index(MORTGAGE30US[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'ASFMA'], index(tail(MORTGAGE30US, 1))
      )))
    )
  )

# Farm mortgage interest divided by GDP
df.data$ASFMA.INTEREST.by.GDP <-
  (df.data$ASFMA.INTEREST / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ASFMA.INTEREST.by.GDP",
      string.source = "Calc",
      string.description = "Farm Mortgages (Quarterly, NSA)\nInterest Burden Divided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'ASFMA.INTEREST'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'ASFMA.INTEREST'], index(tail(GDP, 1))
      )))
    )
  )

# Farm income divided by GDP
df.data$FARMINCOME.by.GDP <-
  (df.data$FARMINCOME / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "FARMINCOME.by.GDP",
      string.source = "Calc",
      string.description = "Farm Income (Annual, NSA)\nDivided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'FARMINCOME'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'FARMINCOME'], index(tail(GDP, 1))
      )))
    )
  )


# Millions to billions
df.data$BOGMBASE <- df.data$BOGMBASE / 1000

# Monetary base (in billions) divided by GDP
df.data$BOGMBASE.by.GDP <-
  (df.data$BOGMBASE / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "BOGMBASE.by.GDP",
      string.source = "Calc",
      string.description = "BOGMBASE\nDivided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'BOGMBASE'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'BOGMBASE'], index(tail(GDP, 1))
      )))
    )
  )


# Millions to billions
df.data$WALCL <- df.data$WALCL / 1000

# Excess reserve balances (in billions) divided by GDP
df.data$WALCL.by.GDP <-
  (df.data$WALCL / df.data$GDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "WALCL.by.GDP",
      string.source = "Calc",
      string.description = "All Federal Reserve Banks: Total Assets\nDivided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'WRESBAL'], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'WRESBAL'], index(tail(GDP, 1))
      )))
    )
  )

# Don't know why, but not all reserve money data series are in billions, some are in millions. Fix this.
df.data$ECBASSETS <- df.data$ECBASSETS / 1000
df.data$EUNNGDP <- df.data$EUNNGDP / 1000

# ECB assets (in billions) divided by European GDP
df.data$ECBASSETS.by.EUNNGDP <-
  (df.data$ECBASSETS / df.data$EUNNGDP) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "ECBASSETS.by.EUNNGDP",
      string.source = "Calc",
      string.description = "Central Bank Assets for Euro Area (11-19 Countries)\nDivided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'ECBASSETS'], index(EUNNGDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'ECBASSETS'], index(tail(EUNNGDP, 1))
      )))
    )
  )

# Yield curve, 30-year to 10-year
df.data$DGS30TO10 <- df.data$DGS30 - df.data$DGS10
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DGS30TO10",
      string.source = "Calc",
      string.description = "Yield Curve, 30 and 10 Year Treasury (DGS30-DGS10)",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(DGS30[1]), index(DGS10[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(DGS30, 1)), index(tail(DGS10, 1))
      )))
    )
  )

# Yield curve, 10-year to 1-year
df.data$DGS10TO1 <- df.data$DGS10 - df.data$DGS1
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DGS10TO1",
      string.source = "Calc",
      string.description = "Yield Curve, 10 and 1 Year Treasury (DGS10-DGS1)",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(DGS10[1]), index(DGS1[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(DGS10, 1)), index(tail(DGS1, 1))
      )))
    )
  )

# Yield curve, 10-year to 2-year
df.data$DGS10TO2 <- df.data$DGS10 - df.data$DGS2
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DGS10TO2",
      string.source = "Calc",
      string.description = "Yield Curve, 10 and 2 Year Treasury (DGS10-DGS2)",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(DGS10[1]), index(DGS2[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(DGS10, 1)), index(tail(DGS2, 1))
      )))
    )
  )

# Yield curve, 10 year to 3-month (Monthly)
df.data$DGS10TOTB3MS <- df.data$DGS10 - df.data$TB3MS
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DGS10TOTB3MS",
      string.source = "Calc",
      string.description = "Yield Curve, 10 and 3 Month Treasury (DGS10-TB3MS)",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(DGS10[1]), index(TB3MS[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(DGS10, 1)), index(tail(TB3MS, 1))
      )))
    )
  )

# Yield curve, 10 year to 3-month (Daily)
df.data$DGS10TODTB3 <- df.data$DGS10 - df.data$DTB3
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DGS10TODTB3",
      string.source = "Calc",
      string.description = "Yield Curve, 10 and 3 Month Treasury (DGS10-DTB3)",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(DGS10[1]), index(DTB3[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(DGS10, 1)), index(tail(DTB3, 1))
      )))
    )
  )


# AAA to 10-year treasury
df.data$DGS10ByAAA <- df.data$AAA / df.data$DGS10
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DGS10ByAAA",
      string.source = "Calc",
      string.description = "AAA ratio to 10 year treasury (AAA/DGS10)",
      string.label.y = "-",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(AAA[1]), index(DGS10[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(AAA, 1)), index(tail(DGS10, 1))
      )))
    )
  )

# Unemployment level (Not seasonally adjusted) to populations. FRED keeps both of these in units of thousands of people
df.data$LNU03000000BYPOPTHM <-
  (df.data$LNU03000000 / df.data$POPTHM) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "LNU03000000BYPOPTHM",
      string.source = "Calc",
      string.description = "Unemployment level (NSA) / Population",
      string.label.y = "%" ,
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(LNU03000000[1]), index(POPTHM[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(LNU03000000, 1)), index(tail(POPTHM, 1))
      )))
    )
  )

# Unemployment level (Seasonally adjusted) to populations. FRED keeps both of these in units of thousands of people
df.data$UNEMPLOYBYPOPTHM <-
  (df.data$UNEMPLOY / df.data$POPTHM) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "UNEMPLOYBYPOPTHM",
      string.source = "Calc",
      string.description = "Unemployment level, seasonally adjusted / Population",
      string.label.y = "%" ,
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(UNEMPLOY[1]), index(POPTHM[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(UNEMPLOY, 1)), index(tail(POPTHM, 1))
      )))
    )
  )

# ADP to populations. FRED keeps both of these in units of thousands of people
df.data$NPPTTLBYPOPTHM <-
  (df.data$NPPTTL / df.data$POPTHM) * 100
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "NPPTTLBYPOPTHM",
      string.source = "Calc",
      string.description = "ADP Private Employment / Population",
      string.label.y = "%" ,
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(NPPTTL[1]), index(POPTHM[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(NPPTTL, 1)), index(tail(POPTHM, 1))
      )))
    )
  )


# U6 to U4 unemployment
df.data$U6toU3 <- df.data$U6RATE - df.data$UNRATE
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "U6toU3",
      string.source = "Calc",
      string.description = "U6RATE minums UNRATE",
      string.label.y = "%" ,
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(U6RATE[1]), index(UNRATE[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(U6RATE, 1)), index(tail(UNRATE, 1))
      )))
    )
  )

# Normalize copper by commodities producer price index (PPIACO)
df.data$CHRISCMEHG1.by.PPIACO <-
  (df.data$CHRISCMEHG1 / df.data$PPIACO)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CHRISCMEHG1.by.PPIACO",
      string.source = "Calc",
      string.description = "Copper, $/lb, Normalized by\ncommodities producer price index",
      string.label.y = "$/lb/Index",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(CHRISCMEHG1[1]), index(PPIACO[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(CHRISCMEHG1, 1)), index(tail(PPIACO, 1))
      )))
    )
  )

# Normalize copper by consumer price index (CPI)
df.data$CHRISCMEHG1.by.CPIAUCSL <-
  (df.data$CHRISCMEHG1 / df.data$CPIAUCSL)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "CHRISCMEHG1.by.CPIAUCSL",
      string.source = "Calc",
      string.description = "Copper, $/lb, Normalized by\nconsumer price index",
      string.label.y = "$/lb/Index",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(CHRISCMEHG1[1]), index(CPIAUCSL[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(CHRISCMEHG1, 1)), index(tail(CPIAUCSL, 1))
      )))
    )
  )

# Normalize crude by producer price index, commodities (PPICO)
df.data$DCOILBRENTEU.by.PPIACO <-
  (df.data$DCOILBRENTEU / df.data$PPIACO)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DCOILBRENTEU.by.PPIACO",
      string.source = "Calc",
      string.description = "Crude Oil - Brent, $/bbl, Normalized by\nproducer price index c.o.",
      string.label.y = "$/bbl/Index",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(DCOILBRENTEU[1]), index(PPIACO[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(DCOILBRENTEU, 1)), index(tail(PPIACO, 1))
      )))
    )
  )

df.data$DCOILWTICO.by.PPIACO <-
  (df.data$DCOILWTICO / df.data$PPIACO)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "DCOILWTICO.by.PPIACO",
      string.source = "Calc",
      string.description = "Crude Oil - WTI, $/bbl, Normalized by\nproducer price index c.o.",
      string.label.y = "$/bbl/Index",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(DCOILWTICO[1]), index(PPIACO[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(DCOILWTICO, 1)), index(tail(PPIACO, 1))
      )))
    )
  )


# Normalize gold by commodities producer price index (PPIACO)
df.data$GOLDAMGBD228NLBM.by.PPIACO <-
  (df.data$GOLDAMGBD228NLBM / df.data$PPIACO)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GOLDAMGBD228NLBM.by.PPIACO",
      string.source = "Calc",
      string.description = "Gold, USD/Troy OUnce, Normalized by\ncommodities producer price index",
      string.label.y = "$/t oz/Index",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GOLDAMGBD228NLBM[1]), index(PPIACO[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GOLDAMGBD228NLBM, 1)), index(tail(PPIACO, 1))
      )))
    )
  )

# Normalize gold by consumer price index (CPI)
df.data$GOLDAMGBD228NLBM.by.CPIAUCSL <-
  (df.data$GOLDAMGBD228NLBM / df.data$CPIAUCSL)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GOLDAMGBD228NLBM.by.CPIAUCSL",
      string.source = "Calc",
      string.description = "Gold, USD/Troy OUnce, Normalized by\nconsumer price index",
      string.label.y = "$/t oz/Index",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GOLDAMGBD228NLBM[1]), index(CPIAUCSL[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GOLDAMGBD228NLBM, 1)), index(tail(CPIAUCSL, 1))
      )))
    )
  )

# Normalize gold by GDP
df.data$GOLDAMGBD228NLBM.by.GDP <-
  (df.data$GOLDAMGBD228NLBM / df.data$GDP)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GOLDAMGBD228NLBM.by.GDP",
      string.source = "Calc",
      string.description = "Gold, USD/Troy OUnce, Normalized by GDP",
      string.label.y = "$/t oz/Index",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GOLDAMGBD228NLBM[1]), index(GDP[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GOLDAMGBD228NLBM, 1)), index(tail(GDP, 1))
      )))
    )
  )

# Normalize GSG (close) commodities by GDP deflator
df.data$GSG.Close.by.GDPDEF <-
  (df.data$GSG.Close / df.data$GDPDEF)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GSG.Close.by.GDPDEF",
      string.source = "Calc",
      string.description = "GSCI Commodity-Indexed Trust,\nNormalized by GDP def",
      string.label.y = "(-)",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GSG[1]), index(GDPDEF[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GSG, 1)), index(tail(GDPDEF, 1))
      )))
    )
  )

# Normalize GSG (close) commodities by S&P 500
df.data$GSG.Close.by.GSPC.Close <-
  (df.data$GSG.Close / df.data$GSPC.Close)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GSG.Close.by.GSPC.Close",
      string.source = "Calc",
      string.description = "GSCI Commodity-Indexed Trust,\nNormalized by S&P 500",
      string.label.y = "(-)",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GSG[1]), index(GSPC[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GSG, 1)), index(tail(GSPC, 1))
      )))
    )
  )


# GDP divided by population
df.data$GDPBYPOPTHM <-
  ((df.data$GDP * 1e9) / (df.data$POPTHM * 1e3))
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GDPBYPOPTHM",
      string.source = "Calc",
      string.description = "GDP/Population",
      string.label.y = "$/person",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GDP[1]), index(POPTHM[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GDP, 1)), index(tail(POPTHM, 1))
      )))
    )
  )

# GDP Divided CPI
df.data$GDPBYCPIAUCSL <- (df.data$GDP / (df.data$CPIAUCSL / 100))
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GDPBYCPIAUCSL",
      string.source = "Calc",
      string.description = "GDP divided by CPI",
      string.label.y = "GDP/CPIAUCSL, Billions of Dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GDP[1]), index(CPIAUCSL[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GDP, 1)), index(tail(CPIAUCSL, 1))
      )))
    )
  )

# Normalize GDP by Population
df.data$GDPBYCPIAUCSLBYPOPTHM <-
  ((df.data$GDPBYCPIAUCSL * 1e9) / (df.data$POPTHM * 1e3))
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GDPBYCPIAUCSLBYPOPTHM",
      string.source = "Calc",
      string.description = "GDP divided by CPI/Population",
      string.label.y = "$/Person",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'GDPBYCPIAUCSL'], index(POPTHM[1]))
      )) ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'GDPBYCPIAUCSL'], index(tail(POPTHM, 1))
      )))
    )
  )

# Normalize S&P 500 by mid-cap
df.data$GSPC.CloseBYMDY.Close <-
  ((df.data$GSPC.Close) / (df.data$MDY.Close))
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GSPC.CloseBYMDY.Close",
      string.source = "Calc",
      string.description = "GSPC by MDY",
      string.label.y = "-",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GSPC[1]), index(MDY[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GSPC, 1)), index(tail(MDY, 1))
      )))
    )
  )

# Normalize NASDAQ by mid-cap
df.data$QQQ.CloseBYMDY.Close <-
  ((df.data$QQQ.Close) / (df.data$MDY.Close))
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "QQQ.CloseBYMDY.Close",
      string.source = "Calc",
      string.description = "QQQ by MDY",
      string.label.y = "-",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(QQQ[1]), index(MDY[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(QQQ, 1)), index(tail(MDY, 1))
      )))
    )
  )

# Calculate the daily swing in GSPC as percentage of opening price
df.data$GSPC.Open[df.data$GSPC.Open <=0] <- 1
df.data$GSPC.DailySwing <-
  ((df.data$GSPC.High - df.data$GSPC.Low) / df.data$GSPC.Open)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GSPC.DailySwing",
      string.source = "Calc",
      string.description = "S&P 500 (^GSPC) Daily Swing: (High - Low) / Open",
      string.label.y = "-",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GSPC[1]), index(GSPC[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GSPC, 1)), index(tail(GSPC, 1))
      )))
    )
  )

# Correct GSPC open with GDP deflator
df.data$GSPC.Open.by.GDPDEF <-
  (df.data$GSPC.Open / (df.data$GDPDEF / 100))
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GSPC.Open.by.GDPDEF",
      string.source = "Calc",
      string.description = "S&P 500 (^GSPC) Open\ndivided by GDP deflator",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GSPC[1]), index(GDPDEF[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GSPC, 1)), index(tail(GDPDEF, 1))
      )))
    )
  )

# Correct GSPC Close with GDP deflator
df.data$GSPC.Close.by.GDPDEF <-
  (df.data$GSPC.Close / (df.data$GDPDEF / 100))
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "GSPC.Close.by.GDPDEF",
      string.source = "Calc",
      string.description = "S&P 500 (^GSPC) Close\ndivided by GDP deflator",
      string.label.y = "Dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GSPC[1]), index(GDPDEF[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GSPC, 1)), index(tail(GDPDEF, 1))
      )))
    )
  )

# Census housing data
df.data$HNFSUSNSA.minus.HSN1FNSA <-
  (df.data$HNFSUSNSA - df.data$HSN1FNSA)
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "HNFSUSNSA.minus.HSN1FNSA",
      string.source = "Calc",
      string.description = "Houses for sale -\n houses sold",
      string.label.y = "Thousands of Units",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'HSN1FNSA'], index(HNFSUSNSA[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'HSN1FNSA'], index(tail(HNFSUSNSA, 1))
      )))
    )
  )

# Estimate housing input to the economy by multiplying starts by median price
df.data$MSPUS.times.HOUST <-
  (df.data$MSPUS * df.data$HOUST)/1000000
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "MSPUS.times.HOUST",
      string.source = "Calc",
      string.description = "New privately owned units start\ntimes median price",
      string.label.y = "Millions of dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'MSPUS'], index(HOUST[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'MSPUS'], index(tail(HOUST, 1))
      )))
    )
  )


# Do all homes that are started come up for sale? Take a look at number of new
# private homes for sale times median price.
df.data$MSPUS.times.HNFSUSNSA <-
  (df.data$MSPUS * df.data$HNFSUSNSA)/1000000
df.symbols <-
  rbind(
    df.symbols,
    data.frame(
      string.symbol = "MSPUS.times.HNFSUSNSA",
      string.source = "Calc",
      string.description = "New privately owned 1-family units for sale\ntimes median price",
      string.label.y = "Millions of dollars",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'MSPUS'], index(HOUST[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'MSPUS'], index(tail(HOUST, 1))
      )))
    )
  )


