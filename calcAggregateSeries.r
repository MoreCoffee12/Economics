
#-------- Check that the columns exist in the data frame-----------------------
require_columns <- function(df, cols ) {
  miss <- setdiff(cols, names(df))
  if (length(miss)){
    FALSE
  }else{
    TRUE
  } 
}


# Create an aggregate for retail sales
if ( require_columns(df.data, c("RRSFS.Value", "RSALES.Value") ) ){
  df.data$RSALESAGG <- 
    rowMeans(df.data[, c("RRSFS.Value", "RSALES.Value")], na.rm = TRUE)
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
        date.series.end = as.Date(index(tail(RRSFS, 1))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
        )
    )
}

# Difference between monthly SA and NSA series
if ( require_columns(df.data, c("BUSLOANS.Value", "BUSLOANSNSA.Value") ) ){
  df.data$BUSLOANS.minus.BUSLOANSNSA <-
    (df.data$BUSLOANS.Value - df.data$BUSLOANSNSA.Value)

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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Difference between monthly SA and NSA series / GDP
if ( require_columns(df.data, c("BUSLOANS.minus.BUSLOANSNSA", "GDP.Value") ) ){
  
  df.data$BUSLOANS.minus.BUSLOANSNSA.by.GDP <-
    (df.data$BUSLOANS.minus.BUSLOANSNSA / df.data$GDP.Value) * 100
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize business loans (monthly, SA) by GDP
if ( require_columns(df.data, c("BUSLOANS.Value", "GDP.Value") ) ){
  
  df.data$BUSLOANS.by.GDP <- (df.data$BUSLOANS.Value / df.data$GDP.Value) * 100
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Business loans (monthly, SA) interest
if ( require_columns(df.data, c("BUSLOANS.Value", "DGS10.Value") ) ){
  
  df.data$BUSLOANS.INTEREST <-
    (df.data$BUSLOANS.Value * df.data$DGS10.Value) / 100
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Business loans interest divided by GDP
if ( require_columns(df.data, c("BUSLOANS.INTEREST", "GDP.Value") ) ){
    
  df.data$BUSLOANS.INTEREST.by.GDP <-
    (df.data$BUSLOANS.INTEREST / df.data$GDP.Value) * 100
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize business loans (monthly, NSA) by GDP
if ( require_columns(df.data, c("BUSLOANSNSA.Value", "GDP.Value") ) ){
  
  df.data$BUSLOANSNSA.by.GDP <-
    (df.data$BUSLOANSNSA.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize business loans (weekly, SA) by GDP
if ( require_columns(df.data, c("TOTCI.Value", "GDP.Value") ) ){
  
  df.data$TOTCI.by.GDP <- (df.data$TOTCI.Value / df.data$GDP.Value) * 100
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize (weekly, NSA) business loans by GDP
if ( require_columns(df.data, c("TOTCINSA.Value", "GDP.Value") ) ){
  
  df.data$TOTCINSA.by.GDP <- (df.data$TOTCINSA.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Business loans (weekly, NSA) interest
if ( require_columns(df.data, c("TOTCINSA.Value", "DGS10.Value") ) ){
  
  df.data$TOTCINSA.INTEREST <- 
    (df.data$TOTCINSA.Value * df.data$DGS10.Value) / 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Business loans (weekly, NSA) interest divided by GDP
if ( require_columns(df.data, c("TOTCINSA.INTEREST", "GDP.Value") ) ){
  
  df.data$TOTCINSA.INTEREST.by.GDP <-
    (df.data$TOTCINSA.INTEREST / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize real personal income by GDP
if ( require_columns(df.data, c("W875RX1.Value", "GDP.Value") ) ){

  df.data$W875RX1.by.GDP <- (df.data$W875RX1.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize NSA personal income by GDP
if ( require_columns(df.data, c("A065RC1A027NBEA.Value", "GDP.Value") ) ){
  
  df.data$A065RC1A027NBEA.by.GDP <-
    (df.data$A065RC1A027NBEA.Value / df.data$GDP.Value) * 100
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize SA personal income by GDP
if ( require_columns(df.data, c("A065RC1A027NBEA.Value", "GDP.Value") ) ){
  
  df.data$PI.by.GDP <- (df.data$PI.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize pre-tax corporate profits by GDP
if ( require_columns(df.data, c("A053RC1Q027SBEA.Value", "GDP.Value") ) ){
  
  df.data$A053RC1Q027SBEA.by.GDP <-
    (df.data$A053RC1Q027SBEA.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize pre-tax corporate profits (with inventory and capital adjustments)
# by GDP
if ( require_columns(df.data, c("CPROFIT.Value", "GDP.Value") ) ){
  
  df.data$CPROFIT.by.GDP <- (df.data$CPROFIT.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Consumer loans as a percent of GDP
if ( require_columns(df.data, c("CONSUMERNSA.Value", "GDP.Value") ) ){
  
  df.data$CONSUMERNSA.by.GDP <-
    (df.data$CONSUMERNSA.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Residential real estate (monthly, NSA) loans as a percent of GDP
if ( require_columns(df.data, c("RREACBM027NBOG.Value", "GDP.Value") ) ){
  
  df.data$RREACBM027NBOG.by.GDP <-
    (df.data$RREACBM027NBOG.Value / df.data$GDP.Value) * 100

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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Residential real estate (monthly, SA) loans as a percent of GDP
if ( require_columns(df.data, c("RREACBM027SBOG.Value", "GDP.Value") ) ){
  
  df.data$RREACBM027SBOG.by.GDP <-
    (df.data$RREACBM027SBOG.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Residential real estate (weekly, SA) loans as a percent of GDP
if ( require_columns(df.data, c("RREACBW027SBOG.Value", "GDP.Value") ) ){
  
  df.data$RREACBW027SBOG.by.GDP <-
    (df.data$RREACBW027SBOG.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Residential real estate (weekly, NSA) loans as a percent of GDP
if ( require_columns(df.data, c("RREACBW027NBOG.Value", "GDP.Value") ) ){
  
  df.data$RREACBW027NBOG.by.GDP <-
    (df.data$RREACBW027NBOG.Value / df.data$GDP.Value) * 100

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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# All durable goods as percent of GDP. Have to convert from millions to
# billions to be consistent with H.8 and GDP series
if ( require_columns(df.data, c("UMDMNO.Value", "GDP.Value") ) ){
  
  df.data$UMDMNO.Value <- df.data$UMDMNO.Value / 1000
  df.data$UMDMNO.by.GDP <-
    (df.data$UMDMNO.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# All durable goods as percet of GDP. Have to convert from millions to
# billions to be consistent with H.8 and GDP series
if ( require_columns(df.data, c("DGORDER", "GDP.Value") ) ){
  
  df.data$DGORDER <- df.data$DGORDER / 1000
  df.data$DGORDER.by.GDP <-
    (df.data$DGORDER.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# All home mortgages as percet of GDP. Have to convert from millions to
# billions to be consistent with H.8 and GDP series
if ( require_columns(df.data, c("ASHMA", "GDP.Value") ) ){
  
  df.data$ASHMA <- df.data$ASHMA / 1000
  df.data$ASHMA.by.GDP <-
    (df.data$ASHMA.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# All home mortgages interest burden
if ( require_columns(df.data, c("ASHMA.Value", "MORTGAGE30US.Value") ) ){
  
  df.data$ASHMA.INTEREST <-
    (df.data$ASHMA.Value * df.data$MORTGAGE30US.Value) / 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# All home mortgages interest burden divided by GDP
if ( require_columns(df.data, c("ASHMA.INTEREST", "GDP.Value") ) ){
  
  df.data$ASHMA.INTEREST.by.GDP <-
    (df.data$ASHMA.INTEREST / df.data$GDP.Value) * 100
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "ASHMA.INTEREST.by.GDP",
        string.source = "Calc",
        string.description = "Home Mortgages (Quarterly, NSA)\n 30-Year Fixed Interest Burdens\nDivided by GDP",
        string.label.y = "PERCENT",
        float.expense.ratio = -1.00,
        date.series.start =  as.Date(max(
          c(df.symbols$date.series.start[df.symbols$Symbol == 'ASHMA.INTEREST'], index(GDP[1]))
        ))  ,
        date.series.end = as.Date(min(c(
          df.symbols$date.series.end[df.symbols$Symbol == 'ASHMA.INTEREST'], index(tail(GDP, 1))
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Consumer loans interest using the 24 month consumer series.
if ( require_columns(df.data, c("CONSUMERNSA.Value", "TERMCBPER24NS.Value") ) ){
  
  df.data$CONSUMERNSA.INTEREST <-
    (df.data$CONSUMERNSA.Value * df.data$TERMCBPER24NS.Value) / 100

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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Total loans interest divided by GDP
if ( require_columns(df.data, c("CONSUMERNSA.INTEREST", "GDP.Value") ) ){
  
  df.data$CONSUMERNSA.INTEREST.by.GDP <-
    (df.data$CONSUMERNSA.INTEREST / df.data$GDP.Value) * 100

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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )

}

# Create aggregate of total loans
if ( require_columns(df.data, c("BUSLOANS.Value", "REALLNNSA.Value", "CONSUMERNSA.Value") ) ){
  
  df.data$TOTLNNSA <-
    (df.data$BUSLOANS.Value + df.data$REALLNNSA.Value + df.data$CONSUMERNSA.Value)
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Total loans as a percent of GDP
if ( require_columns(df.data, c("TOTLNNSA", "GDP.Value") ) ){
  
  df.data$TOTLNNSA.by.GDP <-
    (df.data$TOTLNNSA / df.data$GDP.Value) * 100

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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Total loans interest
if ( require_columns(df.data, c("TOTLNNSA", "DGS10.Value") ) ){
  
  df.data$TOTLNNSA.INTEREST <-
    (df.data$TOTLNNSA * df.data$DGS10.Value) / 100

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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Total loans interest divided by GDP
if ( require_columns(df.data, c("TOTLNNSA.INTEREST", "GDP.Value") ) ){
    
  df.data$TOTLNNSA.INTEREST.by.GDP <-
    (df.data$TOTLNNSA.INTEREST / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}


# Reserve balances (in billions) divided by GDP
if ( require_columns(df.data, c("WRESBAL.Value", "GDP.Value") ) ){
  
  df.data$WRESBAL.by.GDP <-
    (df.data$WRESBAL.Value / df.data$GDP.Value) * 100

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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}


# Don't know why, but not all reserve money data series are in billions, some
# are in millions. Fix this.
if ( require_columns(df.data, c("EXCSRESNW.Value") ) ){
  
  df.data$EXCSRESNW.Value <- df.data$EXCSRESNW.Value / 1000
}

# Excess reserve balances (in billions) divided by GDP
if ( require_columns(df.data, c("EXCSRESNW.Value", "GDP.Value") ) ){
  
  df.data$EXCSRESNW.by.GDP <-
    (df.data$EXCSRESNW.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Federal reserve repos, from millions to billions
# TODO - Update df.symbols with the new units
if ( require_columns(df.data, c("WLRRAL.Value") ) ){
  
  df.data$WLRRAL.Value <- df.data$WLRRAL.Value / 1000

}

# Reverse repos (in billions) divided by GDP
if ( require_columns(df.data, c("WLRRAL.Value", "GDP.Value") ) ){
  
  df.data$WLRRAL.by.GDP <-
    (df.data$WLRRAL.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}


# Secured overnight variation
if ( require_columns(df.data, c("SOFR99.Value", "SOFR1.Value") ) ){
  
  df.data$SOFR99.minus.SOFR1 <-
    (df.data$SOFR99.Value - df.data$SOFR1.Value)
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}


# Census data in millions, needs to be billions.Already fixed in the descriptions
if ( require_columns(df.data, c("IMPCH.Value", "EXPCH.Value") ) ){
  
  df.data$IMPCH.Value <- df.data$IMPCH.Value / 1000
  df.data$EXPCH.Value <- df.data$EXPCH.Value / 1000

}


# China trade balance
if ( require_columns(df.data, c("EXPCH.Value", "IMPCH.Value") ) ){
    
  df.data$EXPCH.minus.IMPCH <-
    (df.data$EXPCH.Value - df.data$IMPCH.Value)
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}


# Millions to billions
# TODO update df.symbols
if ( require_columns(df.data, c("EXPMX.Value", "IMPMX.Value") ) ){
  
  df.data$EXPMX.Value <- df.data$EXPMX.Value / 1000
  df.data$IMPMX.Value <- df.data$IMPMX.Value / 1000

}


# Mexico trade balance
if ( require_columns(df.data, c("EXPMX.Value", "IMPMX.Value") ) ){
  
  df.data$EXPMX.minus.IMPMX <-
    (df.data$EXPMX.Value - df.data$IMPMX.Value)
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Z.1 in millions, needs to be in billions. Already fixed in the descriptions
if ( require_columns(df.data, c("ASTLL.Value", "FBDILNECA.Value") ) ){
  
  df.data$ASTLL.Value <- df.data$ASTLL.Value / 1000
  df.data$FBDILNECA.Value <- df.data$FBDILNECA.Value / 1000
  df.data$ASOLAL.Value <- df.data$ASOLAL.Value / 1000
  df.data$ASTMA.Value <- df.data$ASTMA.Value / 1000
  df.data$ASMRMA.Value <- df.data$ASMRMA.Value / 1000
  df.data$ASCMA.Value <- df.data$ASCMA.Value / 1000

}

# Nonfinancial corporate business; security repurchase agreements; asset, Level
# divided by GDP
if ( require_columns(df.data, c("SRPSABSNNCB.Value", "GDP.Value") ) ){
  
  df.data$SRPSABSNNCB.by.GDP <-
    (df.data$SRPSABSNNCB.Value / df.data$GDP.Value) * 100
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )

}


# Total loans divided by GDP
if ( require_columns(df.data, c("ASTLL.Value", "GDP.Value") ) ){
  
  df.data$ASTLL.by.GDP <-
    (df.data$ASTLL.Value / df.data$GDP.Value) * 100
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Z.1 in millions, needs to be in billions. Already fixed in the descriptions
if ( require_columns(df.data, c("ASFMA.Value") ) ){
  
  df.data$ASFMA.Value <- df.data$ASFMA.Value / 1000
  
}


# Farm loans divided by GDP
if ( require_columns(df.data, c("ASFMA.Value", "GDP.Value") ) ){
  
  df.data$ASFMA.by.GDP <-
    (df.data$ASFMA.Value / df.data$GDP.Value) * 100
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Take a look at farm mortgages as a percentage of total loans.
if ( require_columns(df.data, c("ASFMA.Value", "ASTLL.Value") ) ){
  
  df.data$ASFMA.by.ASTLL <- (df.data$ASFMA.Value / df.data$ASTLL.Value) * 100 
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Farm mortgages interest burden
if ( require_columns(df.data, c("ASFMA.Value", "MORTGAGE30US.Value") ) ){
  
  df.data$ASFMA.INTEREST <-
    (df.data$ASFMA.Value * df.data$MORTGAGE30US.Value) / 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Farm mortgage interest divided by GDP
if ( require_columns(df.data, c("ASFMA.INTEREST", "GDP.Value") ) ){
  
  df.data$ASFMA.INTEREST.by.GDP <-
    (df.data$ASFMA.INTEREST / df.data$GDP.Value) * 100
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}


# Farm income divided by GDP
if ( require_columns(df.data, c("FARMINCOME", "GDP.Value") ) ){
  
  df.data$FARMINCOME.by.GDP <-
    (df.data$FARMINCOME / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}


# Millions to billions
# TODO - Update the df.symbols units to billions
if ( require_columns(df.data, c("BOGMBASE.Value") ) ){
  
  df.data$BOGMBASE.Value <- df.data$BOGMBASE.Value / 1000

}


# Monetary base (in billions) divided by GDP
if ( require_columns(df.data, c("BOGMBASE.Value", "GDP.Value") ) ){
  
  df.data$BOGMBASE.by.GDP <-
    (df.data$BOGMBASE.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Millions to billions
# TODO - Correct the df.symbols table
if ( require_columns(df.data, c("WALCL.Value") ) ){
  
  df.data$WALCL.Value <- df.data$WALCL.Value / 1000

}


# Excess reserve balances (in billions) divided by GDP
if ( require_columns(df.data, c("WALCL.Value", "GDP.Value") ) ){
  
  df.data$WALCL.by.GDP <-
    (df.data$WALCL.Value / df.data$GDP.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Don't know why, but not all reserve money data series are in billions, some
# are in millions. Fix this.
# TODO - Update units in df.symbols
if ( require_columns(df.data, c("ECBASSETS.Value", "EUNNGDP.Value") ) ){

  df.data$ECBASSETS.Value <- df.data$ECBASSETS.Value / 1000
  df.data$EUNNGDP.Value <- df.data$EUNNGDP.Value / 1000
}

# ECB assets (in billions) divided by European GDP
if ( require_columns(df.data, c("ECBASSETS.Value", "EUNNGDP.Value") ) ){
  
  df.data$ECBASSETS.by.EUNNGDP <-
    (df.data$ECBASSETS.Value / df.data$EUNNGDP.Value) * 100
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Yield curve, 30-year to 10-year
if ( require_columns(df.data, c("DGS30.Value", "DGS10.Value") ) ){
  
  df.data$DGS30TO10 <- df.data$DGS30.Value - df.data$DGS10.Value
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}


# Yield curve, 10-year to 1-year
if ( require_columns(df.data, c("DGS10.Value", "DGS1.Value") ) ){
  
  df.data$DGS10TO1 <- df.data$DGS10.Value - df.data$DGS1.Value
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Yield curve, 10-year to 2-year
if ( require_columns(df.data, c("DGS10.Value", "DGS2.Value") ) ){
  
  df.data$DGS10TO2 <- df.data$DGS10.Value - df.data$DGS2.Value
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}


# Yield curve, 10 year to 3-month (Monthly)
if ( require_columns(df.data, c("DGS10.Value", "TB3MS.Value") ) ){
  
  df.data$DGS10TOTB3MS <- df.data$DGS10.Value - df.data$TB3MS.Value
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}


# Yield curve, 10 year to 3-month (Daily)
if ( require_columns(df.data, c("DGS10.Value", "DTB3.Value") ) ){
  
  df.data$DGS10TODTB3 <- df.data$DGS10.Value - df.data$DTB3.Value
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# AAA to 10-year treasury
if ( require_columns(df.data, c("AAA.Value", "DGS10.Value") ) ){
  
  df.data$DGS10ByAAA <- df.data$AAA.Value / df.data$DGS10.Value
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Unemployment level (Not seasonally adjusted) to populations. FRED keeps both
# of these in units of thousands of people.
if ( require_columns(df.data, c("LNU03000000.Value", "POPTHM.Value") ) ){
  
  df.data$LNU03000000BYPOPTHM <-
    (df.data$LNU03000000.Value / df.data$POPTHM.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}


# Unemployment level (Seasonally adjusted) to populations. FRED keeps both of
# these in units of thousands of people.
if ( require_columns(df.data, c("UNEMPLOY.Value", "POPTHM.Value") ) ){
  
  df.data$UNEMPLOYBYPOPTHM <-
    (df.data$UNEMPLOY.Value / df.data$POPTHM.Value) * 100
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# ADP to populations. FRED keeps both of these in units of thousands of people
if ( require_columns(df.data, c("NPPTTL.Value", "POPTHM.Value") ) ){
    
  df.data$NPPTTLBYPOPTHM <-
    (df.data$NPPTTL.Value / df.data$POPTHM.Value) * 100
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# U6 to U4 unemployment
if ( require_columns(df.data, c("U6RATE.Value", "UNRATE.Value") ) ){
  
df.data$U6toU3 <- df.data$U6RATE.Value - df.data$UNRATE.Value
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
      ))),
      string.symbol_safe = safe_symbol_name(string.symbol),
      string.object_name = safe_symbol_name(string.symbol),
      status = "ok",
      error = NA,
      nrows = 0,
      first_date = date.series.start,
      last_date = date.series.end
    )
  )
}


# Normalize crude by producer price index, commodities (PPICO)
if ( require_columns(df.data, c("DCOILBRENTEU.Value", "PPIACO.Value") ) ){
  
  df.data$DCOILBRENTEU.by.PPIACO <-
    (df.data$DCOILBRENTEU.Value / df.data$PPIACO.Value)
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize the oil price by the producer price index
if ( require_columns(df.data, c("DCOILWTICO.Value", "PPIACO.Value") ) ){
  
  df.data$DCOILWTICO.by.PPIACO <-
    (df.data$DCOILWTICO.Value / df.data$PPIACO.Value)
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# # Normalize gold by commodities producer price index (PPIACO)
# df.data$LBMAGOLD.USD_PM.by.PPIACO <-
#   (df.data$LBMAGOLD.USD_PM / df.data$PPIACO)
# df.symbols <-
#   rbind(
#     df.symbols,
#     data.frame(
#       string.symbol = "LBMAGOLD.USD_PM.by.PPIACO",
#       string.source = "Calc",
#       string.description = "Gold, USD PM/Troy Ounce, Normalized by\ncommodities producer price index",
#       string.label.y = "$/t oz/Index",
#       float.expense.ratio = -1.00,
#       date.series.start =  as.Date(max(c(
#         index(LBMAGOLD[1]), index(PPIACO[1])
#       ))) ,
#       date.series.end = as.Date(min(c(
#         index(tail(LBMAGOLD, 1)), index(tail(PPIACO, 1))
#       )))
#     )
#   )

# # Normalize gold by consumer price index (CPI)
# df.data$LBMAGOLD.USD_PM.by.CPIAUCSL <-
#   (df.data$LBMAGOLD.USD_PM / df.data$CPIAUCSL)
# df.symbols <-
#   rbind(
#     df.symbols,
#     data.frame(
#       string.symbol = "LBMAGOLD.USD_PM.by.CPIAUCSL",
#       string.source = "Calc",
#       string.description = "Gold, USD/Troy OUnce, Normalized by\nconsumer price index",
#       string.label.y = "$/t oz/Index",
#       float.expense.ratio = -1.00,
#       date.series.start =  as.Date(max(c(
#         index(LBMAGOLD[1]), index(CPIAUCSL[1])
#       ))) ,
#       date.series.end = as.Date(min(c(
#         index(tail(LBMAGOLD, 1)), index(tail(CPIAUCSL, 1))
#       )))
#     )
#   )

# # Normalize gold by GDP
# df.data$LBMAGOLD.USD_PM.by.GDP <-
#   (df.data$LBMAGOLD.USD_PM / df.data$GDP)
# df.symbols <-
#   rbind(
#     df.symbols,
#     data.frame(
#       string.symbol = "LBMAGOLD.USD_PM.by.GDP",
#       string.source = "Calc",
#       string.description = "Gold, USD/Troy OUnce, Normalized by GDP",
#       string.label.y = "$/t oz/Index",
#       float.expense.ratio = -1.00,
#       date.series.start =  as.Date(max(c(
#         index(LBMAGOLD[1]), index(GDP[1])
#       ))) ,
#       date.series.end = as.Date(min(c(
#         index(tail(LBMAGOLD, 1)), index(tail(GDP, 1))
#       )))
#     )
#   )

# Normalize nominal GDP commodities by GDP deflator
if ( require_columns(df.data, c("GDP.Value", "GDPDEF.Value") ) ){
  
  df.data$GDP.by.GDPDEF <-
    (df.data$GDP.Value / df.data$GDPDEF.Value)
  
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "GDP.by.GDPDEF",
        string.source = "Calc",
        string.description = "Nominal GDP \nNormalized by GDP def",
        string.label.y = "(-)",
        float.expense.ratio = -1.00,
        date.series.start =  as.Date(max(c(
          index(GDP[1]), index(GDPDEF[1])
        ))) ,
        date.series.end = as.Date(min(c(
          index(tail(GDP, 1)), index(tail(GDPDEF, 1))
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize GSG (close) commodities by GDP deflator
if ( require_columns(df.data, c("GSG.Close", "GDPDEF.Value") ) ){
  
  df.data$GSG.Close.by.GDPDEF <-
    (df.data$GSG.Close / df.data$GDPDEF.Value)
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize GSG (close) commodities by S&P 500
if ( require_columns(df.data, c("GSG.Close", "X_GSPC.GSPC.Close") ) ){
  
  df.data$GSG.Close.by.GSPC.Close <-
    (df.data$GSG.Close / df.data$X_GSPC.GSPC.Close)

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
          index(GSG[1]), index(X_GSPC[1])
        ))) ,
        date.series.end = as.Date(min(c(
          index(tail(GSG, 1)), index(tail(X_GSPC, 1))
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}


# GDP divided by population
if ( require_columns(df.data, c("GDP.Value", "POPTHM.Value") ) ){
  
  df.data$GDPBYPOPTHM <-
    ((df.data$GDP.Value * 1e9) / (df.data$POPTHM.Value * 1e3))
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# GDP Divided CPI
if ( require_columns(df.data, c("GDP.Value", "CPIAUCSL.Value") ) ){
  
  df.data$GDPBYCPIAUCSL <- (df.data$GDP.Value / (df.data$CPIAUCSL.Value / 100))
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize GDP by Population
if ( require_columns(df.data, c("GDPBYCPIAUCSL", "POPTHM.Value") ) ){
    
  df.data$GDPBYCPIAUCSLBYPOPTHM <-
    ((df.data$GDPBYCPIAUCSL * 1e9) / (df.data$POPTHM.Value* 1e3))
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize S&P 500 by mid-cap
if ( require_columns(df.data, c("X_GSPC.GSPC.Close", "MDY.Close") ) ){
  
  df.data$GSPC.CloseBYMDY.Close <-
    ((df.data$X_GSPC.GSPC.Close) / (df.data$MDY.Close))

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
          index(X_GSPC[1]), index(MDY[1])
        ))) ,
        date.series.end = as.Date(min(c(
          index(tail(X_GSPC, 1)), index(tail(MDY, 1))
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize NASDAQ by mid-cap
if ( require_columns(df.data, c("QQQ.Close", "MDY.Close") ) ){
  
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Calculate the daily swing in GSPC as percentage of opening price
if ( require_columns(df.data, c("X_GSPC.GSPC.High",
                                "X_GSPC.GSPC.Low","X_GSPC.GSPC.Open") ) ){
  
  df.data$X_GSPC.GSPC.Open[df.data$X_GSPC.GSPC.Open <=0] <- 1
  df.data$GSPC.DailySwing <-
    ((df.data$X_GSPC.GSPC.High - df.data$X_GSPC.GSPC.Low) / df.data$X_GSPC.GSPC.Open)
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
          index(X_GSPC[1]), index(X_GSPC[1])
        ))) ,
        date.series.end = as.Date(min(c(
          index(tail(X_GSPC, 1)), index(tail(X_GSPC, 1))
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Correct GSPC open with GDP deflator
if ( require_columns(df.data, c("X_GSPC.GSPC.Open", "GDPDEF.Value") ) ){
  
  df.data$GSPC.Open.by.GDPDEF <-
    (df.data$X_GSPC.GSPC.Open / (df.data$GDPDEF.Value / 100))
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
          index(X_GSPC[1]), index(GDPDEF[1])
        ))) ,
        date.series.end = as.Date(min(c(
          index(tail(X_GSPC, 1)), index(tail(GDPDEF, 1))
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )

}

# Correct GSPC Close with GDP deflator
if ( require_columns(df.data, c("X_GSPC.GSPC.Close", "GDPDEF.Value") ) ){
  
  df.data$GSPC.Close.by.GDPDEF <-
    (df.data$X_GSPC.GSPC.Close / (df.data$GDPDEF.Value / 100))
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
          index(X_GSPC[1]), index(GDPDEF[1])
        ))) ,
        date.series.end = as.Date(min(c(
          index(tail(X_GSPC, 1)), index(tail(GDPDEF, 1))
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Census housing data
if ( require_columns(df.data, c("HNFSUSNSA.Value", "HSN1FNSA.Value") ) ){
  
  df.data$HNFSUSNSA.minus.HSN1FNSA <-
    (df.data$HNFSUSNSA.Value - df.data$HSN1FNSA.Value)
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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Estimate housing input to the economy by multiplying starts by median price
if ( require_columns(df.data, c("MSPUS.Value", "HOUST.Value") ) ){
  
  df.data$MSPUS.times.HOUST <-
    (df.data$MSPUS.Value * df.data$HOUST.Value)/1000000

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
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# HOUST reports at annual rate, but HOUSTNSA just reports the monthly numbers. I
# scale up the NSA to the annual rate.
if ( require_columns(df.data, c("HOUSTNSA.Value") ) ){
  
  df.data$HOUSTNSA.Value <- df.data$HOUSTNSA.Value * 12

}

# Housing starts divided by population
if ( require_columns(df.data, c("HOUST.Value", "POPTHM.Value") ) ){
  
  df.data$HOUST.div.POPTHM <- (df.data$HOUST.Value / df.data$POPTHM.Value)
  
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "HOUST.div.POPTHM",
        string.source = "Calc",
        string.description = "Housing starts divided\nby U.S. population",
        string.label.y = "Starts per person",
        float.expense.ratio = -1.00,
        date.series.start =  as.Date(max(
          c(df.symbols$date.series.start[df.symbols$Symbol == 'POPTHM'], index(HOUST[1]))
        ))  ,
        date.series.end = as.Date(min(c(
          df.symbols$date.series.end[df.symbols$Symbol == 'POPTHM'], index(tail(HOUST, 1))
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Do all homes that are started come up for sale? Take a look at number of new
# private homes for sale times median price.
if ( require_columns(df.data, c("MSPUS.Value", "HNFSUSNSA.Value") ) ){
  
df.data$MSPUS.times.HNFSUSNSA <-
  (df.data$MSPUS.Value * df.data$HNFSUSNSA.Value)/1000000
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
      ))),
      string.symbol_safe = safe_symbol_name(string.symbol),
      string.object_name = safe_symbol_name(string.symbol),
      status = "ok",
      error = NA,
      nrows = 0,
      first_date = date.series.start,
      last_date = date.series.end
    )
  )

}

# What is the total home sold times median price? HSN1FNSA is in units of
# 'thousands' each month.
if ( require_columns(df.data, c("HSN1FNSA.Value", "EXHOSLUSM495S.Value") ) ){
  
  df.data$MSPUS.times.HSN1FNSA.plusEXHOSLUSM495S <-
    (df.data$MSPUS.Value * (df.data$HSN1FNSA.Value*1000 + df.data$EXHOSLUSM495S.Value))/1e9
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "MSPUS.times.HSN1FNSA.plusEXHOSLUSM495S",
        string.source = "Calc",
        string.description = "Median home price\ntimes new and existing houses sold",
        string.label.y = "Billions of dollars",
        float.expense.ratio = -1.00,
        date.series.start =  as.Date(max(
          c(df.symbols$date.series.start[df.symbols$Symbol == 'MSPUS'], index(HSN1FNSA[1]))
        ))  ,
        date.series.end = as.Date(min(c(
          df.symbols$date.series.end[df.symbols$Symbol == 'MSPUS'], index(tail(HSN1FNSA, 1))
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )
}

# Normalize single-family home sales volume (billions) by GDP
# (which is in billions)
if ( require_columns(df.data, c("MSPUS.times.HSN1FNSA.plusEXHOSLUSM495S",
                                "GDP.Value") ) ){
  
  df.data$MSPUS.times.HSN1FNSA.plusEXHOSLUSM495S.by.GDP <-
    (df.data$MSPUS.times.HSN1FNSA.plusEXHOSLUSM495S / df.data$GDP.Value) * 100
  df.symbols <-
    rbind(
      df.symbols,
      data.frame(
        string.symbol = "MSPUS.times.HSN1FNSA.plusEXHOSLUSM495S.by.GDP",
        string.source = "Calc",
        string.description = "New and existing home sales volume ",
        string.label.y = "Percent",
        float.expense.ratio = -1.00,
        date.series.start =  as.Date(max(c(
          index(BUSLOANS[1]), index(GDP[1])
        )))  ,
        date.series.end = as.Date(min(c(
          index(tail(BUSLOANS, 1)), index(tail(GDP, 1))
        ))),
        string.symbol_safe = safe_symbol_name(string.symbol),
        string.object_name = safe_symbol_name(string.symbol),
        status = "ok",
        error = NA,
        nrows = 0,
        first_date = date.series.start,
        last_date = date.series.end
      )
    )

}

