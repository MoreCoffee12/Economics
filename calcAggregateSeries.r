#------------------------- Unit conversions -----------------------------------
# Federal reserve repos, from millions to billions
# TODO - Update df.symbols with the new units
lst_syms <- c("WLRRAL.Value")
if ( require_columns(df.data, lst_syms ) ){
  
  df.data[[lst_syms[[1]]]] <- ( df.data[[lst_syms[[1]]]] / 1000.0 )
  
}
rm(lst_syms)

# This series must be converted from jobs to thousands of jobs to match the
# previous NPPTTL series. I have updated the units in symbols_catalog.csv 
lst_syms <- c("ADPWNUSNERSA.Value")
if ( require_columns(df.data, lst_syms ) ){
  
  df.data[[lst_syms[[1]]]] <- ( df.data[[lst_syms[[1]]]] / 1000.0 )
  
}
rm(lst_syms)



#------------------------ Create the aggregate series--------------------------
# Create an aggregate column for retail sales (mean of RRSFS.Value and RSALES.Value)
if (require_columns(df.data, c("RRSFS.Value", "RSALES.Value"))) {
  
  # Compute the row-wise mean and write it to a new column
  str.symbol.new <- "RSALESAGG"
  src_cols <- c("RRSFS.Value", "RSALES.Value")
  df.data[[str.symbol.new]] <- rowMeans(df.data[src_cols], na.rm = TRUE)
  
  # Append a metadata row for the derived series to df.symbols
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol       = str.symbol.new,
      string.source       = "Calc",
      string.description  = "Real Retail and Food Services Sales\n(RRSFS and RSALES)",
      string.label.y      = "Millions of Dollars",
      float.expense.ratio = -1.00,
      # Derive start/end from source series (xts): first and last timestamps
      date.series.start   = as.Date(index(RSALES)[1]),
      date.series.end     = as.Date(tail(index(RRSFS), 1)),
      # Safe, syntactic object names for the derived series
      string.symbol_safe  = safe_symbol_name(str.symbol.new),
      string.object_name  = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Cleanup: remove temporary name
  rm(str.symbol.new)
}

# Difference between monthly SA and NSA series
if ( require_columns(df.data, c("BUSLOANS.Value", "BUSLOANSNSA.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "BUSLOANS__minus__BUSLOANSNSA"
  df.data[[str.symbol.new]] <-
    (df.data$BUSLOANS.Value - df.data$BUSLOANSNSA.Value)

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)

}

# Difference between monthly SA and NSA series / GDP
if ( require_columns(df.data, c("BUSLOANS.minus.BUSLOANSNSA", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "BUSLOANS__minus__BUSLOANSNSA__by__GDP"
  df.data[[str.symbol.new]] <-
    (df.data$BUSLOANS.minus.BUSLOANSNSA / df.data$GDP.Value) * 100
 
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)

}

# Normalize business loans (monthly, SA) by GDP
if ( require_columns(df.data, c("BUSLOANS.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "BUSLOANS__by__GDP"
  df.data[[str.symbol.new]] <-
    ( (df.data$BUSLOANS.Value / df.data$GDP.Value) * 100.0 )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Business loans (monthly, SA) interest
if ( require_columns(df.data, c("BUSLOANS.Value", "DGS10.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "BUSLOANS__INTEREST"
  df.data[[str.symbol.new]] <-
    (df.data$BUSLOANS.Value * df.data$DGS10.Value) / 100

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Business loans interest divided by GDP
if ( require_columns(df.data, c("BUSLOANS.INTEREST", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "BUSLOANS__INTEREST__by__GDP"
  df.data[[str.symbol.new]] <-
    (df.data$BUSLOANS__INTEREST / df.data$GDP.Value) * 100

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize business loans (monthly, NSA) by GDP
if ( require_columns(df.data, c("BUSLOANSNSA.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "BUSLOANSNSA__by__GDP"
  df.data[[str.symbol.new]] <-
    (df.data$BUSLOANSNSA.Value / df.data$GDP.Value) * 100

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize business loans (weekly, SA) by GDP
if ( require_columns(df.data, c("TOTCI.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "TOTCI__by__GDP"
  df.data[[str.symbol.new]] <-
    ( (df.data$TOTCI.Value / df.data$GDP.Value) * 100.0 )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize (weekly, NSA) business loans by GDP
if ( require_columns(df.data, c("TOTCINSA.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "TOTCINSA__by__GDP"
  df.data[[str.symbol.new]] <-
    ( (df.data$TOTCINSA.Value / df.data$GDP.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Business loans (weekly, NSA) interest
if ( require_columns(df.data, c("TOTCINSA.Value", "DGS10.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "TOTCINSA__INTEREST"
  df.data[[str.symbol.new]] <-
    ( ( df.data$TOTCINSA.Value * df.data$DGS10.Value) / 100.0 )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Business loans (weekly, NSA) interest divided by GDP
lst.sym <- c("TOTCINSA__INTEREST", "GDP.Value")
if ( require_columns(df.data, lst.sym ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "TOTCINSA__INTEREST__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data[[lst.sym[[1]]]] / df.data[[lst.sym[[2]]]]) * 100.0 )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "Business Loans (weekly, NSA)\nAdjusted Interest Burden Divided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == lst.sym[[1]]], index(GDP[1]))
      ))  ,
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == lst.sym[[1]]], index(tail(GDP, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Tidy memory
rm (lst.sym)

# Normalize real personal income by GDP
if ( require_columns(df.data, c("W875RX1.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "W875RX1__by__GDP"
  df.data[[str.symbol.new]] <-
    ( (df.data$W875RX1.Value / df.data$GDP.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize NSA personal income by GDP
if ( require_columns(df.data, c("A065RC1A027NBEA.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "A065RC1A027NBEA__by__GDP"
  df.data[[str.symbol.new]] <-
    (df.data$A065RC1A027NBEA.Value / df.data$GDP.Value) * 100

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize SA personal income by GDP
if ( require_columns(df.data, c("A065RC1A027NBEA.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "PI__by__GDP"
  df.data[[str.symbol.new]] <- 
    ( ( df.data$PI.Value / df.data$GDP.Value ) * 100.0 )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize pre-tax corporate profits by GDP
if ( require_columns(df.data, c("A053RC1Q027SBEA.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "A053RC1Q027SBEA__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$A053RC1Q027SBEA.Value / df.data$GDP.Value) * 100.0 )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize pre-tax corporate profits (with inventory and capital adjustments)
# by GDP
if ( require_columns(df.data, c("CPROFIT.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "CPROFIT__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$CPROFIT.Value / df.data$GDP.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Consumer loans as a percent of GDP
if ( require_columns(df.data, c("CONSUMERNSA.Value", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "CONSUMERNSA__by__GDP"
  df.data[[str.symbol.new]] <-
    (df.data$CONSUMERNSA.Value / df.data$GDP.Value) * 100
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Residential real estate (monthly, NSA) loans as a percent of GDP
if ( require_columns(df.data, c("RREACBM027NBOG.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "RREACBM027NBOG__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$RREACBM027NBOG.Value / df.data$GDP.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )  
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Residential real estate (monthly, SA) loans as a percent of GDP
if ( require_columns(df.data, c("RREACBM027SBOG.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "RREACBM027SBOG__by__GDP"
  df.data[[str.symbol.new]] <-
    (df.data$RREACBM027SBOG.Value / df.data$GDP.Value) * 100
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Residential real estate (weekly, SA) loans as a percent of GDP
if ( require_columns(df.data, c("RREACBW027SBOG.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "RREACBW027SBOG__by__GDP"
  df.data[[str.symbol.new]] <-
    (df.data$RREACBW027SBOG.Value / df.data$GDP.Value) * 100
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Residential real estate (weekly, NSA) loans as a percent of GDP
if ( require_columns(df.data, c("RREACBW027NBOG.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "RREACBW027NBOG__by__GDP"
  df.data[[str.symbol.new]] <-
    (df.data$RREACBW027NBOG.Value / df.data$GDP.Value) * 100
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# All durable goods as percent of GDP. Have to convert from millions to
# billions to be consistent with H.8 and GDP series
if ( require_columns(df.data, c("UMDMNO.Value", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "UMDMNO__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( ( df.data$UMDMNO.Value / 1000.0) / df.data$GDP.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# All durable goods as percet of GDP. Have to convert from millions to
# billions to be consistent with H.8 and GDP series
if ( require_columns(df.data, c("DGORDER", "GDP.Value") ) ){

    # Add the aggregate to the main data frame
  str.symbol.new <- "DGORDER__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( ( df.data$DGORDER.Value / 1000.0 ) / df.data$GDP.Value) * 100.0 )
  
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# All home mortgages as percent of GDP. Have to convert from millions to
# billions to be consistent with H.8 and GDP series
lst.sym <- c("ASHMA.Value", "GDP.Value") 
if ( require_columns(df.data, lst.sym ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "ASHMA__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( ( df.data[[lst.sym[[1]]]] / 1000.0 ) / df.data[[lst.sym[[2]]]]) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# All home mortgages interest burden
if ( require_columns(df.data, c("ASHMA.Value", "MORTGAGE30US.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "ASHMA__INTEREST"
  df.data[[str.symbol.new]] <-
    (df.data$ASHMA.Value * df.data$MORTGAGE30US.Value) / 100
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# All home mortgages interest burden divided by GDP
if ( require_columns(df.data, c("ASHMA__INTEREST", "GDP.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "ASHMA__INTEREST__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$ASHMA__INTEREST / df.data$GDP.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Consumer loans interest using the 24 month consumer series.
if ( require_columns(df.data, c("CONSUMERNSA.Value", "TERMCBPER24NS.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "CONSUMERNSA__INTEREST"
  df.data[[str.symbol.new]] <-
    ( ( df.data$CONSUMERNSA.Value * df.data$TERMCBPER24NS.Value) / 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Total loans interest divided by GDP
if ( require_columns(df.data, c("CONSUMERNSA__INTEREST", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "CONSUMERNSA__INTEREST__by__GDP"
  df.data[[str.symbol.new]] <-
    (df.data$CONSUMERNSA__INTEREST / df.data$GDP.Value) * 100
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Create aggregate of total loans
if ( require_columns(df.data, c("BUSLOANS.Value", "REALLNNSA.Value",
                                "CONSUMERNSA.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "TOTLNNSA"
  df.data[[str.symbol.new]] <-
    ( df.data$BUSLOANS.Value + df.data$REALLNNSA.Value +
        df.data$CONSUMERNSA.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Total loans as a percent of GDP
if ( require_columns(df.data, c("TOTLNNSA", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "TOTLNNSA__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$TOTLNNSA / df.data$GDP.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Total loans interest
if ( require_columns(df.data, c("TOTLNNSA", "DGS10.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "TOTLNNSA__INTEREST"
  df.data[[str.symbol.new]] <-
    ( ( df.data$TOTLNNSA * df.data$DGS10.Value) / 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Total loans interest divided by GDP
if ( require_columns(df.data, c("TOTLNNSA__INTEREST", "GDP.Value") ) ){
    
  # Add the aggregate to the main data frame
  str.symbol.new <- "TOTLNNSA__INTEREST__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$TOTLNNSA__INTEREST / df.data$GDP.Value ) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Reserve balances (in billions) divided by GDP
if ( require_columns(df.data, c("WRESBAL.Value", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "WRESBAL__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$WRESBAL.Value / df.data$GDP.Value ) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Don't know why, but not all reserve money data series are in billions, some
# are in millions. Fix this.
if ( require_columns(df.data, c("EXCSRESNW.Value") ) ){
  
  df.data$EXCSRESNW.Value <- df.data$EXCSRESNW.Value / 1000
}

# Excess reserve balances (in billions) divided by GDP
if ( require_columns(df.data, c("EXCSRESNW.Value", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "EXCSRESNW__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$EXCSRESNW.Value / df.data$GDP.Value ) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Normalize the FINRA margin debt by the GDP
lst_syms <- c("FINRA_MarginDebt", "GDPDEF.Value")

if ( require_columns(df.data,lst_syms ) ){
  
  # Perform the calculation
  str_sym_new <- "FINRAMarginDebt__by__GDPDEF" 
  df.data[[str_sym_new]] <-
    (df.data[[lst_syms[[1]]]] / df.data[[lst_syms[[2]]]])

  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str_sym_new,
      string.source = "Calc",
      string.description = "Margin debt normalized by GDP deflator",
      string.label.y = "Dollars (Real)",
      float.expense.ratio = -1.00,
      Max030 = FALSE,
      Max180 = FALSE,
      date.series.start =  as.Date(max(c(
        index(FINRA_MarginDebt[1]), index(GDPDEF[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(FINRA_MarginDebt, 1)), index(tail(GDPDEF, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str_sym_new),
      string.object_name = safe_symbol_name(str_sym_new)
    )
  )  

  # Tidy up memory
  if( exists("str_sym_new")){
    rm(str_sym_new)
  }
}

# Tidy up memory
if( exists("lst_syms")){
  rm(lst_syms)
}

# Reverse repos (in billions) divided by GDP
if ( require_columns(df.data, c("WLRRAL.Value", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "WLRRAL__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$WLRRAL.Value / df.data$GDP.Value ) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  if( exists("str.symbol.new")){
    rm(str.symbol.new)
  }
  
}


# Secured overnight variation
if ( require_columns(df.data, c("SOFR99.Value", "SOFR1.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "SOFR99__minus__SOFR1"
  df.data[[str.symbol.new]] <-
    ( df.data$SOFR99.Value - df.data$SOFR1.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Census data in millions, needs to be billions.Already fixed in the descriptions
if ( require_columns(df.data, c("IMPCH.Value", "EXPCH.Value") ) ){
  
  df.data$IMPCH.Value <- df.data$IMPCH.Value / 1000
  df.data$EXPCH.Value <- df.data$EXPCH.Value / 1000

}


# China trade balance
if ( require_columns(df.data, c("EXPCH.Value", "IMPCH.Value") ) ){
    
  # Add the aggregate to the main data frame
  str.symbol.new <- "EXPCH__minus__IMPCH"
  df.data[[str.symbol.new]] <-
    ( df.data$EXPCH.Value - df.data$IMPCH.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Millions to billions
# TODO update df.symbols
if ( require_columns(df.data, c("EXPMX.Value", "IMPMX.Value") ) ){
  
  df.data$EXPMX.Value <- df.data$EXPMX.Value / 1000
  df.data$IMPMX.Value <- df.data$IMPMX.Value / 1000

}


# Mexico trade balance
if ( require_columns(df.data, c("EXPMX.Value", "IMPMX.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "EXPMX__minus__IMPMX"
  df.data[[str.symbol.new]] <-
    ( df.data$EXPMX.Value - df.data$IMPMX.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
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
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "SRPSABSNNCB__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$SRPSABSNNCB.Value / df.data$GDP.Value ) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "Nonfinancial corporate business;\nsecurity repurchase agreements;\nasset, Level (NSA)\nDivided by GDP",
      string.label.y = "PERCENT",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(
        c(df.symbols$date.series.start[df.symbols$Symbol == 'SRPSABSNNCB'], index(GDP[1]))
      )),
      date.series.end = as.Date(min(c(
        df.symbols$date.series.end[df.symbols$Symbol == 'SRPSABSNNCB'], index(tail(GDP, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Total loans divided by GDP
if ( require_columns(df.data, c("ASTLL.Value", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "ASTLL__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$ASTLL.Value / df.data$GDP.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Z.1 in millions, needs to be in billions. Already fixed in the descriptions
if ( require_columns(df.data, c("ASFMA.Value") ) ){
  
  df.data$ASFMA.Value <- df.data$ASFMA.Value / 1000
  
}


# Farm loans divided by GDP
if ( require_columns(df.data, c("ASFMA.Value", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "ASFMA__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$ASFMA.Value / df.data$GDP.Value ) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Take a look at farm mortgages as a percentage of total loans.
if ( require_columns(df.data, c("ASFMA.Value", "ASTLL.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "ASFMA__by__ASTLL"
  df.data[[str.symbol.new]] <- 
    ( ( df.data$ASFMA.Value / df.data$ASTLL.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Farm mortgages interest burden
if ( require_columns(df.data, c("ASFMA.Value", "MORTGAGE30US.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "ASFMA__INTEREST"
  df.data[[str.symbol.new]] <-
    (df.data$ASFMA.Value * df.data$MORTGAGE30US.Value) / 100
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Farm mortgage interest divided by GDP
if ( require_columns(df.data, c("ASFMA__INTEREST", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "ASFMA__INTEREST__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$ASFMA__INTEREST / df.data$GDP.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Farm income divided by GDP
if ( require_columns(df.data, c("FARMINCOME", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "FARMINCOME__by__GDP"
  df.data[[str.symbol.new]] <-
    ( (df.data$FARMINCOME / df.data$GDP.Value ) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Millions to billions
# TODO - Update the df.symbols units to billions
if ( require_columns(df.data, c("BOGMBASE.Value") ) ){
  
  df.data$BOGMBASE.Value <- df.data$BOGMBASE.Value / 1000

}

# Monetary base (in billions) divided by GDP
if ( require_columns(df.data, c("BOGMBASE.Value", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "BOGMBASE__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$BOGMBASE.Value / df.data$GDP.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Millions to billions
# TODO - Correct the df.symbols table
if ( require_columns(df.data, c("WALCL.Value") ) ){
  
  df.data$WALCL.Value <- df.data$WALCL.Value / 1000

}


# Excess reserve balances (in billions) divided by GDP
if ( require_columns(df.data, c("WALCL.Value", "GDP.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "WALCL__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$WALCL.Value / df.data$GDP.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
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
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "ECBASSETS__by__EUNNGDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data$ECBASSETS.Value / df.data$EUNNGDP.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Yield curve, 30-year to 10-year
if ( require_columns(df.data, c("DGS30.Value", "DGS10.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "DGS30TO10"
  df.data[[str.symbol.new]] <- ( df.data$DGS30.Value - df.data$DGS10.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Yield curve, 10-year to 1-year
if ( require_columns(df.data, c("DGS10.Value", "DGS1.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "DGS10TO1"
  df.data[[str.symbol.new]] <- ( df.data$DGS10.Value - df.data$DGS1.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Yield curve, 10-year to 2-year
if ( require_columns(df.data, c("DGS10.Value", "DGS2.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "DGS10TO2"
  df.data[[str.symbol.new]] <- ( df.data$DGS10.Value - df.data$DGS2.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Yield curve, 10 year to 3-month (Monthly)
if ( require_columns(df.data, c("DGS10.Value", "TB3MS.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "DGS10TOTB3MS"
  df.data[[str.symbol.new]] <- ( df.data$DGS10.Value - df.data$TB3MS.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Yield curve, 10 year to 3-month (Daily)
if ( require_columns(df.data, c("DGS10.Value", "DTB3.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "DGS10TODTB3"
  df.data[[str.symbol.new]] <- ( df.data$DGS10.Value - df.data$DTB3.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# AAA to 10-year treasury
if ( require_columns(df.data, c("AAA.Value", "DGS10.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "DGS10ByAAA"
  df.data[[str.symbol.new]] <- ( df.data$AAA.Value / df.data$DGS10.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Unemployment level (Not seasonally adjusted) to populations. FRED keeps both
# of these in units of thousands of people.
if ( require_columns(df.data, c("LNU03000000.Value", "POPTHM.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "LNU03000000BYPOPTHM"
  df.data[[str.symbol.new]] <-
    ( ( df.data$LNU03000000.Value / df.data$POPTHM.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Unemployment level (Seasonally adjusted) to populations. FRED keeps both of
# these in units of thousands of people.
if ( require_columns(df.data, c("UNEMPLOY.Value", "POPTHM.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "UNEMPLOYBYPOPTHM"
  df.data[[str.symbol.new]] <-
    ( ( df.data$UNEMPLOY.Value / df.data$POPTHM.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# ADP to populations. FRED keeps both of these in units of thousands of people
if ( require_columns(df.data, c("NPPTTL.Value", "POPTHM.Value") ) ){
    
  # Add the aggregate to the main data frame
  str.symbol.new <- "NPPTTLBYPOPTHM"
  df.data[[str.symbol.new]] <-
    ( ( df.data$NPPTTL.Value / df.data$POPTHM.Value) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# U6 to U4 unemployment
if ( require_columns(df.data, c("U6RATE.Value", "UNRATE.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "U6toU3"
  df.data[[str.symbol.new]] <- ( df.data$U6RATE.Value - df.data$UNRATE.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}


# Normalize crude by producer price index, commodities (PPICO)
if ( require_columns(df.data, c("DCOILBRENTEU.Value", "PPIACO.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "DCOILBRENTEU__by__PPIACO"
  df.data[[str.symbol.new]] <-
    ( df.data$DCOILBRENTEU.Value / df.data$PPIACO.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize the oil price by the producer price index
lst.syms <- c("DCOILWTICO.Value", "PPIACO.Value")
if ( require_columns(df.data, lst.syms ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "DCOILWTICO__by__PPIACO"
  df.data[[str.symbol.new]] <-
    ( df.data[[lst.syms[[1]]]] / df.data[[lst.syms[[2]]]] )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}else{
  print(paste("Failed to find: ", lst.syms))
}

# Free up memory
rm(lst.syms)

# Normalize COMEX gold by consumer price index (CPI)
lst.syms <- c("GC_F.Close", "PPIACO.Value")
if ( require_columns(df.data, lst.syms ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "GC_F.Close__by__PPIACO.Value"    
  df.data[[str.symbol.new]] <-
    ( df.data[[lst.syms[[1]]]] / df.data[[lst.syms[[2]]]] )

  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "Gold, USD/Troy OUnce, Normalized by\nconsumer price index",
      string.label.y = "$/t oz/Index",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GC_F[1]), index(PPIACO[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GC_F, 1)), index(tail(PPIACO, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}else{
  print(paste("Failed to find: ", lst.syms))
}

# Free up memory
rm(lst.syms)

# Normalize gold by GDP deflator
lst.syms <- c("GC_F.Close", "GDPDEF.Value")

if ( require_columns(df.data, lst.syms ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "GC_F.Close__by__GDPDEF.Value"    
  df.data[[str.symbol.new]] <-
    ( df.data[[lst.syms[[1]]]] / df.data[[lst.syms[[2]]]] )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "Gold, USD/Troy Ounce, Normalized by GDP Deflator",
      string.label.y = "$/t oz (Real Dollars)",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GC_F[1]), index(PPIACO[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GC_F, 1)), index(tail(GDPDEF, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}else{
  print(paste("Failed to find: ", lst.syms))
}

# Free up memory
rm(lst.syms)




# Normalize GLD ETF gold by consumer price index (CPI)
lst.syms <- c("GLD.Close", "PPIACO.Value")
if ( require_columns(df.data, lst.syms ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "GLD.Close__by__PPIACO.Value"    
  df.data[[str.symbol.new]] <-
    ( df.data[[lst.syms[[1]]]] / df.data[[lst.syms[[2]]]] )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "GLD, USD, Normalized by\nconsumer price index",
      string.label.y = "$/t oz/Index",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GLD[1]), index(PPIACO[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GLD, 1)), index(tail(PPIACO, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}else{
  print(paste("Failed to find: ", lst.syms))
}

# Free up memory
rm(lst.syms)

# Normalize GLD by GDP deflator
lst.syms <- c("GLD.Close", "GDPDEF.Value")

if ( require_columns(df.data, lst.syms ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "GLD.Close__by__GDPDEF.Value"    
  df.data[[str.symbol.new]] <-
    ( df.data[[lst.syms[[1]]]] / df.data[[lst.syms[[2]]]] )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "Gold, USD, Normalized by GDP Deflator",
      string.label.y = "$/t oz (Real Dollars)",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(GLD[1]), index(PPIACO[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(GLD, 1)), index(tail(GDPDEF, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}else{
  print(paste("Failed to find: ", lst.syms))
}

# Free up memory
rm(lst.syms)







# Normalize nominal GDP commodities by GDP deflator
if ( require_columns(df.data, c("GDP.Value", "GDPDEF.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "GDP__by__GDPDEF"
  df.data[[str.symbol.new]] <-
    ( df.data$GDP.Value / df.data$GDPDEF.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize GSG (close) commodities by GDP deflator
if ( require_columns(df.data, c("GSG.Close", "GDPDEF.Value") ) ){
  
  str.symbol.new <- "GSG__Close__by__GDPDEF"
  df.data[[str.symbol.new]] <-
    (df.data$GSG.Close / df.data$GDPDEF.Value)
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize GSG (close) commodities by S&P 500
if ( require_columns(df.data, c("GSG.Close", "X_GSPC.GSPC.Close") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "GSG__Close__by__GSPC__Close"
  df.data[[str.symbol.new]] <-
    ( df.data$GSG.Close / df.data$X_GSPC.GSPC.Close )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# GDP divided by population
if ( require_columns(df.data, c("GDP.Value", "POPTHM.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "GDPBYPOPTHM"
  df.data[[str.symbol.new]] <-
    ( ( df.data$GDP.Value * 1e9) / ( df.data$POPTHM.Value * 1e3 ) )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# GDP Divided CPI
if ( require_columns(df.data, c("GDP.Value", "CPIAUCSL.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "GDPBYCPIAUCSL"
  df.data[[str.symbol.new]] <- 
    ( df.data$GDP.Value / ( df.data$CPIAUCSL.Value / 100 ) )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize GDP by Population
if ( require_columns(df.data, c("GDPBYCPIAUCSL", "POPTHM.Value") ) ){

  # Add the aggregate to the main data frame
  str.symbol.new <- "GDPBYCPIAUCSLBYPOPTHM"
  df.data[[str.symbol.new]] <- 
    ((df.data$GDPBYCPIAUCSL * 1e9) / (df.data$POPTHM.Value* 1e3))
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize S&P 500 by mid-cap
if ( require_columns(df.data, c("X_GSPC.GSPC.Close", "MDY.Close") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "X_GSPC__Close__by__MDY__Close"
  df.data[[str.symbol.new]] <- 
    ( df.data$X_GSPC.GSPC.Close / df.data$MDY.Close )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "GSPC divided by MDY",
      string.label.y = "-",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(X_GSPC[1]), index(MDY[1])
      ))) ,
      date.series.end = as.Date(min(c(
        index(tail(X_GSPC, 1)), index(tail(MDY, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize NASDAQ by mid-cap
if ( require_columns(df.data, c("QQQ.Close", "MDY.Close") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "QQQ__CloseBYMDY__Close"
  df.data[[str.symbol.new]] <-
    ( df.data$QQQ.Close / df.data$MDY.Close )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Calculate the daily swing in GSPC as percentage of opening price
if ( require_columns(df.data, c("X_GSPC.GSPC.High",
                                "X_GSPC.GSPC.Low","X_GSPC.GSPC.Open") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "X_GSPC__DailySwing"
  df.data$X_GSPC.GSPC.Open[df.data$X_GSPC.GSPC.Open <=0] <- 1
  df.data[[str.symbol.new]] <-
    ((df.data$X_GSPC.GSPC.High - df.data$X_GSPC.GSPC.Low) / df.data$X_GSPC.GSPC.Open)
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Correct GSPC open with GDP deflator
if ( require_columns(df.data, c("X_GSPC.GSPC.Open", "GDPDEF.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "X_GSPC__Open__by__GDPDEF"
  df.data[[str.symbol.new]] <-
    ( df.data$X_GSPC.GSPC.Open / ( df.data$GDPDEF.Value / 100.0 ) )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Correct GSPC Close with GDP deflator
if ( require_columns(df.data, c("X_GSPC.GSPC.Close", "GDPDEF.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "X_GSPC__Close__by__GDPDEF"
  df.data[[str.symbol.new]] <-
    ( df.data$X_GSPC.GSPC.Close / ( df.data$GDPDEF.Value / 100.0 ) )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Census housing data
if ( require_columns(df.data, c("HNFSUSNSA.Value", "HSN1FNSA.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "HNFSUSNSA__minus__HSN1FNSA"
  df.data[[str.symbol.new]] <-
    ( df.data$HNFSUSNSA.Value - df.data$HSN1FNSA.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Estimate housing input to the economy by multiplying starts by median price
if ( require_columns(df.data, c("MSPUS.Value", "HOUST.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "MSPUS__times__HOUST"
  df.data[[str.symbol.new]] <-
    ( ( df.data$MSPUS.Value * df.data$HOUST.Value ) / 1000000.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# HOUST reports at annual rate, but HOUSTNSA just reports the monthly numbers. I
# scale up the NSA to the annual rate.
if ( require_columns(df.data, c("HOUSTNSA.Value") ) ){
  
  df.data$HOUSTNSA.Value <- df.data$HOUSTNSA.Value * 12

}

# Housing starts divided by population
if ( require_columns(df.data, c("HOUST.Value", "POPTHM.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "HOUST__div__POPTHM"
  df.data[[str.symbol.new]] <- ( df.data$HOUST.Value / df.data$POPTHM.Value )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Do all homes that are started come up for sale? Take a look at number of new
# private homes for sale times median price.
if ( require_columns(df.data, c("MSPUS.Value", "HNFSUSNSA.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "MSPUS__times__HNFSUSNSA"
  df.data[[str.symbol.new]] <-
    ( ( df.data$MSPUS.Value * df.data$HNFSUSNSA.Value ) / 1000000.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# What is the total home sold times median price? HSN1FNSA is in units of
# 'thousands' each month.
if ( require_columns(df.data, c("MSPUS.Value", "HSN1FNSA.Value",
                                "EXHOSLUSM495S.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "MSPUS__times__HSN1FNSA__plus__EXHOSLUSM495S"
  df.data[[str.symbol.new]] <-
    ( ( df.data$MSPUS.Value * ( df.data$HSN1FNSA.Value * 1000 +
                                df.data$EXHOSLUSM495S.Value ) ) / 1e9 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Normalize single-family home sales volume (billions) by GDP
# (which is in billions)
lst.sym <- c("MSPUS__times__HSN1FNSA__plus__EXHOSLUSM495S", "GDP.Value")
if ( require_columns(df.data, lst.sym ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "MSPUS__times__HSN1FNSA__plus__EXHOSLUSM495S__by__GDP"
  df.data[[str.symbol.new]] <-
    ( ( df.data[[lst.sym[[1]]]] / df.data[[lst.sym[[2]]]]) * 100.0 )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
      string.source = "Calc",
      string.description = "New and existing home sales volume ",
      string.label.y = "Percent",
      float.expense.ratio = -1.00,
      date.series.start =  as.Date(max(c(
        index(MSPUS[1]), index(GDP[1])
      )))  ,
      date.series.end = as.Date(min(c(
        index(tail(MSPUS, 1)), index(tail(GDP, 1))
      ))),
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Initial claims (ICSA) divided by population
if ( require_columns(df.data, c("ICSA.Value", "POPTHM.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "ICSA__by__POPTHM"
  df.data[[str.symbol.new]] <-
    ( ( df.data$GDP.Value * 1e9) / ( df.data$POPTHM.Value * 1e3 ) )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# continuing claims (CCSA) divided by population
if ( require_columns(df.data, c("CCSA.Value", "POPTHM.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "CCSA__by__POPTHM"
  df.data[[str.symbol.new]] <-
    ( ( df.data$GDP.Value * 1e9) / ( df.data$POPTHM.Value * 1e3 ) )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# Initial claims (ICSA) divided by civilian working population
if ( require_columns(df.data, c("ICSA.Value", "CLF16OV.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "ICSA__by__CLF16OV"
  df.data[[str.symbol.new]] <-
    ( ( df.data$GDP.Value * 1e9) / ( df.data$POPTHM.Value * 1e3 ) )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}

# continuing claims (CCSA) divided by population
if ( require_columns(df.data, c("CCSA.Value", "CLF16OV.Value") ) ){
  
  # Add the aggregate to the main data frame
  str.symbol.new <- "CCSA__by__CLF16OV"
  df.data[[str.symbol.new]] <-
    ( ( df.data$GDP.Value * 1e9) / ( df.data$POPTHM.Value * 1e3 ) )
  
  # Update the symbols table    
  df.symbols <- symbols_append_row(
    df.symbols,
    list(
      string.symbol = str.symbol.new,
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
      string.symbol_safe = safe_symbol_name(str.symbol.new),
      string.object_name = safe_symbol_name(str.symbol.new)
    )
  )
  
  # Tidy memory
  rm(str.symbol.new)
  
}
