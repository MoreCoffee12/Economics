#' Add quarter lines to plot
#'
#' @param my.plot Handle to the ggplot object
#' @param dt.start.year Year the lines should start
#' @param i.count Number of lines
#'
#' @return
#' @export
#'
#' @examples
add.quarter.lines <- function(my.plot, dt.start.year = 2016, i.count = 200){
  x <- as.yearqtr(dt.start.year + seq(0, i.count) / 4)
  x.date <- as.Date(x) - 1
  my.plot + geom_vline(xintercept = as.numeric(x.date), linetype = 4)
}

#' Formats long formulas so they can be displayed in a plot header
#'
#' @param str.input Formula to be formatted
#'
#' @return
#' @export
#'
#' @examples
get.formula.formatted <-
  function(str.input = "result ~ test+test1+test2+test3+test4") {
    # Initialize output string
    str.out = ''
    
    # Split up the string
    str.split <- strsplit(str.input, "[+]")
    i.count = 0
    
    # Reconstruct, but with newline after the 3rd '+'
    for (str.list in str.split[[1]]) {
      if (i.count > 0) {
        if (i.count == 3) {
          str.out = paste(str.out, str.list, sep = "\n")
        }
        else{
          str.out = paste(str.out, str.list, sep = "+")
        }
      } else{
        str.out = str.list
      }
      i.count <- i.count + 1
    }
    
    return(str.out)
  }

#' Return a string describing the slope of a data series 
#'
#' @param df.data Data frame with the data
#' @param datay Name of column to operate on
#' @param days How many days to include
#'
#' @return str.trend: string describing the slope
#' @export
#'
#' @examples
getTrendString <- function(df.data, datay, days) {
  # datay <- "DGS10TOTB3MS"
  df.data.fit <- data.frame(tail(df.data[datay], days))
  df.data.fit$datax <- as.double(tail(df.data$date, days))
  colnames(df.data.fit) <- c("datay", "datax")
  
  mdl.linear <- lm(datay ~ datax, data = df.data.fit)
  df.data.fit$pred <- predict(mdl.linear, newdata = df.data.fit)
  
  # Turn these lines on to visualize the results.
  # my.plot <-
  #    ggplot(data = df.data.fit, aes(datax, datay)) + geom_line()
  #  my.plot + geom_line(data = df.data.fit, aes(datax, pred), colour = "red")
  
  # Select the string
  str.trend <- "flat"
  if (mdl.linear$coefficients[[2]]/mean(df.data.fit$datay) < -0.001) {
    str.trend <- "negative"
  }
  if (mdl.linear$coefficients[[2]]/mean(df.data.fit$datay) > 0.001) {
    str.trend <- "positive"
  }
  
  return(str.trend)
  
}




#' This function provides a common format for the plots
#'
#' @param datadf_rec Dataframe with recession indicator 
#' @param datadf Datafrom with the data  
#' @param datax Horizontal axis column name. Almost always a date
#' @param datay Vertical axis column name.
#' @param titlelabel String describing the plot title
#' @param xlabel String for the horizontal axis label
#' @param ylabel String for the vertical axis label
#' @param xlim Plotting limits for the vertical axis
#' @param ylim Plotting limits for the horizontal axis
#' @param b.legend If true then plot includes a legend
#' @param b.percentile If true then 5%/95% percentile lines are displayed
#' @param b.long.legend Defaults to FASLE. If set to TRUE then legend entries include symbol description.
#'
#' @return Handle to the plot
#' @export
#'
#' @examples
plotSingle <-
  function(datadf_rec,
           datadf,
           datax,
           datay,
           titlelabel,
           xlabel,
           ylabel,
           xlim,
           ylim,
           b.legend,
           b.percentile,
           b.long.legend = FALSE) {
    
  # From http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
  # The palette with black:
  cbbPalette <-
    c(
      "#000000",
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7"
    )
  # The palette with grey:
  cbPalette <-
    c(
      "#999999",
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7"
    )  
  
  # Create the start date string
  str.start = getMostRecentDateString(df.symbols, datay)
  
  # Create the legend entry
  str.legend <- datay
  if (b.long.legend){
    str.legend <- getPlotTitle(df.symbols, datay, str.sep = "\n")
    
  }
  
  # Construct the plot itself
  my.plot <- ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour = "grey", size = 0.5)) +
    geom_line(
      data = datadf,
      aes_string(
        x = datax,
        y = datay,
        colour = factor(str.legend)
      ),
      na.rm = TRUE,
      size = 0.7
    ) +
    scale_colour_manual(values = cbbPalette) +
    guides(
      colour = guide_legend("Series"),
      size = guide_legend("Series"),
      shape = guide_legend("Series")
    ) +
    geom_rect(
      data = datadf_rec,
      aes(
        xmin = start,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "grey",
      alpha = 0.3,
      na.rm = TRUE
    ) +
    scale_fill_continuous(name = "V") +
    #geom_smooth(method = "lm") +
    ggtitle(titlelabel) +
    labs(x = paste(xlabel, " (", str.start, ")", sep = ""), y = ylabel) +
    scale_x_date(limits = xlim) +
    scale_y_continuous(limits = ylim) +
    if (b.legend) {
      if (b.long.legend) {
        theme(legend.position = "right",
              legend.key.size = unit(1.5, "cm"),
        )
      } else{
        theme(legend.position = "top")
      }
    } else{
      theme(legend.position = "none")
    }
  
  # Apply percentile markers
  if (!missing(b.percentile)) {
    # It is present, should it be included?
    if (b.percentile) {
      data.stats <-
        datadf[datadf$date > as.Date(xlim[1]) &
                 datadf$date < as.Date(xlim[2]), datay]
      d.quant <- quantile(data.stats, c(0.05, 0.95))
      my.plot <-
        my.plot + annotate(
          "text",
          label = "5%",
          x = xlim[1],
          y = (d.quant[[1]] - abs(d.quant[[1]] * 0.05)),
          colour = "black",
          vjust = 1,
          hjust = 1,
          size = 3.5
        )
      my.plot <-
        my.plot + geom_hline(yintercept = d.quant[[1]],
                             linetype = "dashed",
                             color = "grey")
      my.plot <-
        my.plot + annotate(
          "text",
          label = "95%",
          x = xlim[1],
          y = (d.quant[[2]] + abs(d.quant[[1]] * 0.05)),
          colour = "black",
          vjust = 0,
          hjust = 1,
          size = 3.5
        )
      my.plot <-
        my.plot + geom_hline(yintercept = d.quant[[2]],
                             linetype = "dashed",
                             color = "grey")
    }
  }
  
  return(my.plot)
  }


#' Return color-blind friendly pallette with black
#'
#' @return Vector with colors
#' @export
#'
#' @examples
getPaletteBlack <- function() {
  # From http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
  
  cbbPalette <-
    c(
      "#000000",
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7"
    )
  
  return(cbbPalette)
  
}

#' Return color-blind friendly pallette with grey
#'
#' @return Vector with colors
#' @export
#'
#' @examples
getPaletteGrey <- function() {
  # From http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
  
  cbPalette <-
    c(
      "#999999",
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7"
    )
  
  return(cbPalette)
  
}


#' Plot a single series bench mark
#'
#' @param datadf_rec Dataframe with recession indicator 
#' @param datay Column with data to be plotted against the benchmark
#' @param ylim Vertical plotting limits
#' @param df.symbols Data frame with the symbol information
#' @param df.data Data frame with the financial series
#' @param string.analysis.start Starting data for the analysis
#'
#' @return
#' @export
#'
#' @examples
plotSingleBench <-
  function(datadf_rec, datay,
           ylim,
           df.symbols,
           df.data,
           string.analysis.start) {
    # The palette with black:
    cbbPalette <- getPaletteBlack()
    
    if (grep('.', datay) > 0) {
      strSym <- strSymOnly(datay)
      strTitle <-
        paste(datay, " | ", df.symbols[grep(strSym, df.symbols$string.symbol),]$string.description)
      strYLabel <-
        df.symbols[grep(strSym, df.symbols$Symbol),]$string.label.y
    } else{
      strTitle <-
        paste(datay, " | ", df.symbols[grep(datay, df.symbols$string.symbol),]$string.description)
      strYLabel <-
        df.symbols[grep(datay, df.symbols$Symbol),]$string.label.y
    }
    dataBench <- "GSPC.Close_Norm"
    datax <- "date"
    my.plot <- plotSingle(datadf_rec, df.data,
                          datax,
                          datay,
                          strTitle,
                          "Date",
                          strYLabel,
                          c(as.Date(string.analysis.start), Sys.Date()),
                          ylim,
                          TRUE)
    # Label the end point for the data being compared to the benchmark
    d.y.value <- tail(df.data[[datay]], 1)
    my.plot <-
      my.plot + annotate(
        geom = "point",
        x = tail(df.data$date, 1),
        y = d.y.value,
        color = cbbPalette[2],
        size = 1.75
      )
    my.plot <-
      my.plot + annotate(
        geom = "text",
        x = tail(df.data$date, 1),
        y = d.y.value,
        label = format(d.y.value, digits = 3, nsmall = 2),
        vjust = 0.5,
        hjust = -0.1,
        color = cbbPalette[2]
      )
    
    my.plot <-
      my.plot + geom_line(
        data = df.data,
        aes_string(
          x = datax,
          y = dataBench,
          colour = factor(dataBench)
        ),
        na.rm = TRUE,
        size = 0.7
      )
    
    return(my.plot)
    
    
  }

#' Calculate efficiency frontier, plot return versus volatility
#'
#' @param string.portfolio.in Name of the portfolio to plot
#' @param df.data.in Data frame with the time history for the stock symbols
#' @param df.symbols.in Data frame with meta data for the stock symbols
#'
#' @return List: dfRR (data frame of symbols in portfolio),
#'               myPlot (handle to time series plot)
#'               plot_dt (efficiency frontier)
#'
#' @export
#'
#' @examples
plotReturnVolatility <-
  function(string.portfolio.in,
           df.data.in,
           df.symbols.in) {
    dfRR <- df.symbols.in[df.symbols.in[string.portfolio.in] > 0, ]
    
    # Need a data table with just the ticker and data
    strCols <-
      paste(dfRR$string.symbol, ".Close_Norm_YoY", sep = "")
    dfPort <- data.table(df.data.in[, strCols])
    
    # Range of expected returns from the porfolio
    er_vals <-
      seq(
        from = min(dfRR$ExpReturn),
        to = max(dfRR$ExpReturn),
        length.out = 1000
      )
    
    # find an optimal portfolio for each possible possible expected return
    # (note that the values are explicitly set between the minimum and maximum of
    # the expected returns per asset)
    sd_vals <- rep(0, length(er_vals))
    tryCatch({
      sd_vals <- sapply(er_vals, function(er) {
        op <- portfolio.optim(as.matrix(dfPort), er)
        return(op$ps)
      })
    }, error = function(e) {
      print("Failed to find efficiency boundary")
    })
    
    # Collect in a table
    plot_dt <- data.table(sd = sd_vals, er = er_vals)
    
    # find the lower and the upper frontier
    minsd <- min(plot_dt$sd)
    minsd_er <- plot_dt[sd == minsd, er]
    minsd_er <- minsd_er[1]
    plot_dt[, efficient := er >= minsd_er]
    
    # Data for the actual portfolio mix
    dfPortRet <- dfData[string.portfolio.in]
    strPfYoY <- paste(string.portfolio.in, "_YoY", sep = "")
    dfPortRet[strPfYoY] <-
      CalcYoY(dfPortRet, string.portfolio.in, iRetPd)
    Vol_pf <- sd(dfPortRet[, strPfYoY])
    ExpRet_pf <- mean(dfPortRet[, strPfYoY])
    #print(Vol_pf)
    #print(ExpRet_pf)
    
    # Data for the portfolio mix with same returns, but lower volatility
    op_pf <- portfolio.optim(as.matrix(dfPort), ExpRet_pf)
    dfRR[paste(string.portfolio.in, "_Opt", sep = "")] <- op_pf$pw
    
    # Plot the data
    myPlot <- ggplot() +
      # Plot the portfolio elements
      geom_point(data = dfRR,
                 aes(
                   x = Volatility,
                   y = ExpReturn,
                   color = string.symbol
                 ),
                 size = 3,
                 shape=15) +
      labs(color = "Symbol") +
      # Plot the efficiency frontier
      geom_line(data = plot_dt[efficient == F],
                aes(x = sd, y = er),
                size = 0.5,
                color = "blue") +
      geom_line(data = plot_dt[efficient == T],
                aes(x = sd, y = er),
                size = 0.5,
                color = "red") +
      annotate(geom = "point",
               x = Vol_pf,
               y = ExpRet_pf,
               size = 5
      ) +
      annotate(geom = "text",
               label = "As-found",
               x = Vol_pf,
               y = ExpRet_pf,
               hjust = -0.15,
               vjust = 0.5,
               size = 4
      ) +
      annotate(geom = "point",
               x = op_pf$ps,
               y = ExpRet_pf,
               size = 5,
               color = "blue"
      ) +
      annotate(geom = "text",
               label = "Optimized",
               x = op_pf$ps,
               y = ExpRet_pf,
               hjust = 1.15,
               vjust = 0.5,
               size = 4
      ) +
      theme_bw() + ggtitle("Risk-Return Tradeoff (Red/Blue=Efficient), Annual Returns") +
      guides(fill=guide_legend(title="New Legend Title")) +
      xlab("Volatility") +
      ylab("Expected Returns") +
      scale_y_continuous(label = scales::percent, limits = c(-0.05, 0.1)) +
      scale_x_continuous(label = scales::percent, limits = c(-0.05, 0.25))
    
    #myPlot$labels$colour <- "Symbol"
    
    return(list(dfRR, myPlot, plot_dt))
    
  }


# ------------------------------------------------------------------------------
#' Define the function for calculating year over year growth. 
#'
#' @param datadf Data frame with date and data
#' @param strCol Column name for calculations
#' @param iPeriods Number of periods. Defaults to 2
#'
#' @return Array with YoY data
#' @export
#'
#' @examples
CalcYoY <- function (datadf, strCol, iPeriods){
  
  if (missing(datadf)){
    datadf <- data.frame(c(1, 2, 3, 4, 5))
    colnames(datadf) <- "USREC"
  }
  
  if (missing(strCol)) {
    strCol <- "USREC"
  }
  
  if (missing(iPeriods)) {
    iPeriods <- 2
  }
  
  
  Nrow <- nrow(datadf)
  GrowthRateYoY <- rep(0,Nrow)
  GrowthRateYoY[(iPeriods+1):Nrow] <- diff(as.matrix(datadf[[strCol]]), lag = iPeriods)
  #GrowthRateYoY <- (GrowthRateYoY / datadf[[strCol]])*100
  GrowthRateYoY <- (GrowthRateYoY / shift(as.matrix(datadf[[strCol]]), n=iPeriods, type="lag"))*100
  GrowthRateYoY[is.na(GrowthRateYoY)] = 0
  return(GrowthRateYoY)
}


#' Get the root ticker symbol (for those with .open, .close, etc.)
#'
#' @param df.symbols Dataframe with the symbols
#' @param datay String with the ticker symbol
#'
#' @return string with the root ticker symbol
#' @export
#'
#' @examples
getRootTickerSymbol <-function(datay){
  str.search <- datay
  if (grepl('.', datay)) {
    # Ticker symbols have .open, .close, etc. that are not in df.symbols. For
    # these symbols, search for the parent symbol
    str.search <- strsplit(datay, "[.]")[[1]][1]
  }
  
  return(str.search)  
} 

#' Small helper function to get the symbol description
#'
#' @param df.symbols Dataframe with the symbols
#' @param datay String describing the symbol
#'
#' @return string with plot title
#' @export
#'
#' @examples
getPlotTitle <- function(df.symbols, datay, str.sep = " | "){
  # Default ticker description
  str.desc <- df.symbols[grep(paste("^", datay, "$", sep=""), df.symbols$string.symbol),]$string.description

  # if it is empty, see if the root ticker has the info
  if (length(str.desc) <= 0) {
    # Find the search string
    str.search <- getRootTickerSymbol(datay)
    
    # Search for the data
    str.desc <- df.symbols[grep(paste("^", str.search, "$", sep=""), df.symbols$string.symbol),]$string.description
  }
  
  strTitle <-  paste(datay, str.sep, str.desc, sep="" )
  return(strTitle)
}

#' Get the formatted string for the y-axis plot
#'
#' @param df.symbols Dataframe with the symbols
#' @param datay String describing the symbol
#'
#' @return
#' @export
#'
#' @examples
getPlotYLabel <- function(df.symbols, datay) {
  
  # Pick up the y-axis description, default to the inputted ticker symbol
  str.y <-
    df.symbols[grep(paste("^", datay, "$", sep = ""), df.symbols$string.symbol),]$string.label.y
  
  # if it is empty, see if the root ticker has the info
  if( length(str.y) <=0 ){
    # Find the search string
    str.search <- getRootTickerSymbol(datay)
    
    # Execute the search
    str.y <-
      df.symbols[grep(paste("^", str.search, "$", sep = ""), df.symbols$string.symbol),]$string.label.y
  }

  # Return the string
  return(str.y)
}


#' Get the 'Recent Date' string for the x-axis label
#'
#' @param df.symbols Dataframe with the symbols
#' @param datay String with the ticker symbol
#'
#' @return string with the formatted recent date information
#' @export
#'
#' @examples
getMostRecentDateString <- function(df.symbols, datay) {
  # Pick up the y-axis description, default to the inputted ticker symbol
  str.recent.date <-
    as.character(df.symbols[df.symbols$string.symbol == datay, "date.series.end"])
  
  # if it is empty, see if the root ticker has the info
  if (length(str.recent.date) <= 0) {
    # Find the search string
    str.search <- getRootTickerSymbol(datay)
    
    # Search for the data
    str.recent.date <-
      as.character(df.symbols[df.symbols$string.symbol == str.search, "date.series.end"])
  }
  
  # Create the start date string
  str.start <-
    paste("Most recent data: ", str.recent.date, sep = "")
  
  # Return the string
  return(str.start)
}


plotSingleQuickRecent <- function(datay, ylim){
  
  plotSingle(dfRecession, df.data, "date", datay, 
             paste(datay, " | ", df.symbols[grep(paste("^", datay, "$", sep=""), df.symbols$string.symbol),]$string.description), "Date", 
             df.symbols[grep(datay, df.symbols$string.symbol),]$string.label.y, c(as.Date("1jan2000","%d%b%Y"), Sys.Date()), ylim, FALSE)
  
}

plotSingleQuickRecentRecent <- function(datay, ylim){
  
  plotSingle(dfRecession, df.data, "date", datay, 
             paste(datay, " | ", df.symbols[grep(paste("^", datay, "$", sep=""), df.symbols$string.symbol),]$string.description), "Date", 
             df.symbols[grep(datay, df.symbols$string.symbol),]$string.label.y, c(as.Date("1jan2010","%d%b%Y"), Sys.Date()), ylim, FALSE)
  
}

#' Plot of data in modern times, roughly the since the U.S. went off the gold standard.
#'
#' @param datay String with the ticker symbol
#' @param ylim Plotting limits for the horizontal axis
#' @param b.percentile If true then 5%/95% percentile lines are displayed
#'
#' @return
#' @export
#'
#' @examples
plotSingleQuickModern <- function(datay, ylim, b.percentile) {
  b.legend <- FALSE
  if (missing(b.percentile)) {
    b.percentile <- FALSE
  }
  plotSingle(
    dfRecession,
    df.data,
    "date",
    datay,
    titlelabel = getPlotTitle(df.symbols, datay),
    "Date",
    ylabel = getPlotYLabel(df.symbols, datay),
    c(as.Date("1jan1970", "%d%b%Y"), Sys.Date()),
    ylim,
    b.legend,
    b.percentile
  )
  
}


#' Minimum function to plot data.
#'
#' @param datadf_rec Dataframe with recession indicator 
#' @param datadf Datafrom with the data  
#' @param datay String with the ticker symbol
#' @param ylim Plotting limits for the horizontal axis
#' @param dt.start Start date. End data is assumed to be today's date
#' @param b.legend If true then plot includes a legend
#' @param b.percentile If true then 5%/95% percentile lines are displayed
#' @param b.long.legend If true then plot includes a legend at the side with long text
#'
#' @return
#' @export
#'
#' @examples
plotSingleQuick <-
  function(datadf_rec,
           datadf,
           datay,
           ylim,
           dt.start,
           b.legend,
           b.percentile,
           b.long.legend) {
    if (missing(dt.start)) {
      dt.start <- as.Date("1jan1945", "%d%b%Y")
    }
    if (missing(b.legend)) {
      b.legend <- FALSE
    }
    if (missing(b.percentile)) {
      b.percentile <- FALSE
    }
    if (missing(b.long.legend)) {
      b.long.legend <- FALSE
    }
    plotSingle(
      datadf_rec,
      datadf,
      "date",
      datay,
      titlelabel = getPlotTitle(df.symbols, datay),
      xlabel = "Date",
      ylabel = getPlotYLabel(df.symbols, datay),
      c(dt.start, Sys.Date()),
      ylim,
      b.legend,
      b.percentile,
      b.long.legend
    )
    
  }

#' Plots the single pane of predicted returns
#'
#' @param datadf_rec Dataframe with recession indicator 
#' @param datadf Datafrom with the data  
#' @param datay String with the ticker symbol
#' @param ylim Plotting limits for the horizontal axis
#' @param dfPred Data frame with prediction end and start dates
#'
#' @return
#' @export
#'
#' @examples
plotSingleBackPred <- function(datadf_rec,
                               datadf, datay, ylim, dfPred) {
  plotSingle(
    datadf_rec,
    datadf,
    "date",
    datay,
    paste(datay, " | ", df.symbols[grep(paste("^", datay, "$", sep =
                                                ""), df.symbols$string.symbol), ]$string.description),
    "Date",
    df.symbols[grep(datay, df.symbols$string.symbol), ]$string.label.y,
    c(as.Date("1jan1945", "%d%b%Y"), Sys.Date()),
    ylim,
    FALSE
  ) +
    geom_rect(
      data = dfPred,
      aes(
        xmin = predStart,
        xmax = predEnd,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "red",
      alpha = 0.2,
      na.rm = TRUE
    )
  
}

#' Plot the single pane of back-testing growth.
#'
#' @param datadf_rec Dataframe with recession indicator 
#' @param datadf Datafrom with the data  
#' @param datay String with the ticker symbol
#' @param ylim Plotting limits for the horizontal axis
#' @param dt.start Start date. End data is assumed to be today's date
#'
#' @return
#' @export
#'
#' @examples
plotSingleBackGrowth <-
  function(datadf_rec,
           datadf, datay, ylim, dt.start = as.Date('1945-01-01')) {
    myPlot <- plotSingle(
      datadf_rec,
      datadf,
      "date",
      datay,
      paste(datay, " | ", df.symbols[grep(datay, df.symbols$string.symbol), ]$string.description),
      "Date",
      df.symbols[grep(datay, df.symbols$string.symbol), ]$string.label.y,
      c(dt.start, Sys.Date()),
      ylim,
      TRUE
    ) +
      geom_line(
        data = datadf,
        aes_string(
          x = "date",
          y = "eqBase",
          colour = shQuote("eqBase")
        ),
        na.rm = TRUE
      )
    
    return(myPlot)
    
  }

#This bit of code defines the X verus Y plot
# This link was helpful
# http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html
# Define the generic single plane plot function
plotXvY <- function(df.data, dfRecession, datax, datay, titlelabel, xlabel, ylabel, xlim, ylim, bLegend, bFitLinear, 
                    dtStart, b.reverse.y){
  
  # Select data of interest
  dfPlot <- df.data[df.data$date>=dtStart,]
  
  # Include date range
  titlelabel <- paste(titlelabel, "\nDate range: ", dtStart, " to ", tail(dfPlot$date, n=1), "(black since last recession)", sep="")
  dtRec <- tail(dfRecession$initEnd,1)
  
  # Include a linear fit?
  if( bFitLinear ){
    formFit <- as.formula(paste(datay, '~', datax))
    lm_fit <- lm(formFit, data = dfPlot)
    titlelabel <- paste(titlelabel, "\nAdj R2=", signif(summary(lm_fit)$adj.r.squared, 5), 
                        " | Intercept =",signif(lm_fit$coef[[1]],5 ), 
                        " | Slope =",signif(lm_fit$coef[[2]], 5), sep="")
  }
  
  myplot <- ggplot()+
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
    theme(panel.grid.major.x = element_line(colour="grey", size=0.5)) +
    theme(panel.grid.major.y = element_line(colour="grey", size=0.5)) +
    geom_point(data=dfPlot[dfPlot$date<dtRec,], aes_string(x=datax, y=datay),
               na.rm = TRUE, size = 0.7, color="grey") +
    geom_point(data=dfPlot[dfPlot$date>dtRec,], aes_string(x=datax, y=datay), 
               na.rm = TRUE, size = 0.7, color="black") +
    scale_colour_manual(values = c("black","blue", "green")) +
    guides(colour = guide_legend("Series"), size = guide_legend("Series"), shape = guide_legend("Series")) +
    scale_fill_continuous(name = "V") +
    ggtitle(titlelabel) +
    labs(x=xlabel, y = ylabel) +
    scale_x_continuous(limits = xlim ) +
    if( bLegend ){
      theme(legend.position = "top")
    }else{
      theme(legend.position = "none")
    }
    if (b.reverse.y) {
      myplot <- myplot + scale_y_continuous(limits = ylim) 
      myplot <- myplot + scale_y_reverse() 
    }else{
      myplot <- myplot + scale_y_continuous(limits = ylim) 
    } 
    
  if( bFitLinear ){ 
    myplot <- myplot+stat_smooth(data=dfPlot, aes_string(x=datax, y=datay, colour = factor(datay)), method = "lm", col = "red") 
  }
  return(myplot)
}


#' Used for back testing trading strategies
#'
#' @param datadf_rec Dataframe with recession indicator 
#' @param datadf Datafrom with the data  
#' @param dataTrade Series name of the trading moves
#' @param dataRet Series name for the returns
#' @param dataEq Series name for return on equity
#' @param dfPred Data frame with prediction end and start dates
#' @param bOverlay True to overlay
#' @param dtStart Start date
#' @param ylimBackTest Limits for the back test returns
#'
#' @return
#' @export
#'
#' @examples
plotBack <-
  function (datadf_rec,
            datadf,
            dataTrade,
            dataRet,
            dataEq,
            dfPred,
            bOverlay = FALSE,
            dtStart = as.Date('1945-01-01'),
            ylimBackTest = c(0, 1000)) {
    ylim <- c(0, 1)
    if (bOverlay) {
      p1 <- plotSingleBackPred(datadf_rec,
                               datadf, dataTrade, ylim, dfPred)
    }
    else{
      p1 <- plotSingleQuick(datadf_rec, datadf, dataTrade, ylim, dtStart)
    }
    
    ylim <- c(-0.1, 0.1)
    p2 <-
      plotSingleQuick(datadf_rec, datadf, dataRet, ylim, dtStart)
    
    
    p3 <- plotSingleBackGrowth (datadf_rec,
                                datadf, dataEq, ylimBackTest, dtStart)
    
    dPercent <-
      100 * (tail(datadf[, dataEq], 1) - tail(datadf$eqBase, 1)) / tail(datadf$eqBase, 1)
    
    grid.arrange(
      p1,
      p2,
      p3,
      ncol = 1,
      top = paste(
        "Recession Initiation Rule | Baseline Growth = ",
        sprintf('%0.2f', tail(datadf$eqBase, 1)),
        " | Rule Growth = ",
        sprintf('%0.2f', tail(datadf[, dataEq], 1)),
        "\nPercent Improvement: ",
        sprintf('%0.1f%s', dPercent, "%") ,
        sep = ""
      )
    )
    
  }


#' Rolling Correlation
#' This function calculates the rolling correlation (360-day window), adds it to the dataframe, and plots it out.
#'
#' @param datadf_rec Dataframe with recession indicator 
#' @param datadf Datafrom with the data  
#' @param df.symbols Symbols data frame
#' @param datay1 Name of first column to operate on
#' @param ylim1 Vertical limits for the first plot
#' @param datay2 Name of second column to operate on
#' @param ylim2 Vertical limits for the second plot
#' @param wIn Window length in days
#' @param dtStart Start date
#'
#' @return
#' @export
#'
#' @examples
calcRollingCorr <-
  function(datadf_rec,
           datadf,
           df.symbols,
           datay1,
           ylim1,
           datay2,
           ylim2,
           wIn,
           dtStart) {
    # Default for window
    if (missing(wIn)) {
      wIn <- 365
    }
    
    # Default for the start date for the plot. The rolling correlation is calculated for the entire data range.
    if (missing(dtStart)) {
      dtStart <- as.Date("1jan1945", "%d%b%Y")
      
    }
    
    # The calculation
    corrName <- paste(datay1, "_CORR_", datay2, sep = "")
    df.symbols <- df.symbols[!df.symbols$string.symbol == corrName,]
    # Normalization
    data.1 <-
      datadf[, datay1] / (max(datadf[, datay1]) - min(datadf[, datay1]))
    data.2 <-
      datadf[, datay2] / (max(datadf[, datay2]) - min(datadf[, datay2]))
    datadf[, corrName] <- RollingCorr(data.1, data.2, window = wIn)
    datadf[, corrName][is.na(datadf[, corrName])] <- 1
    
    # Establish data
    date.temp.1 <-
      df.symbols[grep(paste("^", datay1, "$", sep = ""), df.symbols$string.symbol),]$date.series.start
    date.temp.2 <-
      df.symbols[grep(paste("^", datay2, "$", sep = ""), df.symbols$string.symbol),]$date.series.start
    date.temp <- max(date.temp.1, date.temp.2)
    
    date.temp.1 <-
      df.symbols[grep(paste("^", datay1, "$", sep = ""), df.symbols$string.symbol),]$date.series.end
    date.temp.2 <-
      df.symbols[grep(paste("^", datay2, "$", sep = ""), df.symbols$string.symbol),]$date.series.end
    date.temp.end <- min(date.temp.1, date.temp.2)
    
    # Update symboles meta data
    df.symbols <-
      rbind(
        df.symbols,
        data.frame(
          string.symbol = corrName,
          string.source = "calc",
          string.description = paste("Rolling Correlation ", wIn, " Day Window"),
          string.label.y = "-" ,
          float.expense.ratio = -1.00,
          Max030 = FALSE,
          Max180 = FALSE,
          date.series.start = date.temp,
          date.series.end = date.temp.end
        )
      )
    
    # Return the updated dataframes to the global enviro
    assign('df.data', datadf, envir = .GlobalEnv)
    assign('df.symbols', df.symbols, envir = .GlobalEnv)
    
    # Plot it out
    p1 <- plotSingleQuick(datadf_rec,
                          datadf, datay1, ylim1, dtStart)
    p2 <- plotSingleQuick(datadf_rec,
                          datadf, datay2, ylim2, dtStart)
    p3 <- plotSingleQuick(datadf_rec,
                          datadf, corrName, c(-1, 1), dtStart)
    
    g1 <- ggplotGrob(p1)
    g2 <- ggplotGrob(p2)
    g3 <- ggplotGrob(p3)
    g <- rbind(g1, g2, g3, size = "first")
    g$widths <- grid::unit.pmax(g1$widths, g2$widths, g3$widths)
    grid.newpage()
    grid.draw(g)
    
    # Return the new name
    return(corrName)
    
  }

#' Find and plot similar periods of data
#'
#' @param df.data Data frame with the data
#' @param df.rec Data frame with recession periods
#' @param df.symbols Data frame with symbols
#' @param datay Time series to operate on
#' @param ylim Vertical plotting limets
#' @param i.window Number of samples to include in the analysis window
#' @param dt.end (Optional) End data for the window. Defaults to current system time
#'
#' @return Handle to the plot
#' @export
#'
#' @examples 
#' my.data <- plotSimilarPeriods(df.data, dfRecession, df.symbols, "UNRATE", c(2,5), 720)
plotSimilarPeriods <-
  function(df.data,
           df.rec,
           df.symbols,
           datay,
           ylim,
           i.window,
           dt.end) {
    # Default for window
    if (missing(dt.end)) {
      dt.end <- Sys.Date()
    }
    
    # Define start points and build the series for rolling correlation. 
    # A storage column called 'similar' is created
    # and will be used in the cross-correlation below.
    dt.start <- dt.end - i.window
    df.data$similar <- 0
    d.data.origin <-
      as.matrix(df.data[(df.data$date >= dt.start) &
                          (df.data$date <= dt.end), datay])
    df.data[(df.data$date >= dt.start) &
              (df.data$date <= dt.end), 'similar'] <- d.data.origin
    # Perform the correlation
    lag.max.segment <- as.integer(length(df.data[,datay]) / 2)
    test <-
      ccf(
        df.data['similar'],
        df.data[datay],
        ylab = "Test",
        type = "correlation",
        lag.max = lag.max.segment,
        plot = FALSE
      )
    idx.max <- which.is.max(test$acf)
    i.offset <- as.integer((dt.end - dt.start) / 2)
    
    # Plot it out
    datax <- "date"
    titlelabel <- getPlotTitle(df.symbols, datay)
    xlabel <- "Date"
    ylabel <- getPlotYLabel(df.symbols, datay)
    xlim <- c(dt.start, dt.end)
    b.legend <- TRUE
    b.percentile <- FALSE
    
    # The most recent data set
    my.plot <-
      plotSingle(
        df.rec,
        df.data,
        datax,
        datay,
        titlelabel,
        xlabel,
        ylabel,
        xlim,
        ylim,
        b.legend,
        b.percentile
      )
    
    # Next most recent
    dt.start.similar <- as.Date(df.data$date[idx.max] - i.offset)
    dt.end.similar <- as.Date(df.data$date[idx.max] + i.offset)

    d.data.segment <- (as.matrix(df.data[(df.data$date >= dt.start.similar) &
                                           (df.data$date <= dt.end.similar), datay]))
    d.data.origin.pkpk <- max(d.data.origin) - min(d.data.origin)
    d.data.segment.pkpk <- max(d.data.segment) - min(d.data.segment)
    if(d.data.segment.pkpk < 1e-10){
      d.data.segment.pkpk <- mean(d.data.segment)
    }
    d.data.segment <- (d.data.segment * d.data.origin.pkpk / d.data.segment.pkpk)
    d.vertical.offset <-
      (df.data[df.data$date == dt.start, datay] - d.data.segment[1])
    
    df.data[(df.data$date >= dt.start) &
              (df.data$date <= dt.end), 'similar'] <-
      d.data.segment + d.vertical.offset 
    my.plot <- my.plot +
      geom_line(
        data = df.data,
        aes_string(
          x = "date",
          y = "similar",
          colour = shQuote(paste(dt.start.similar, " to ", dt.end.similar, sep=""))
        ),
        na.rm = TRUE
      )

    return(list(my.plot, dt.start.similar, dt.end.similar))
  }

