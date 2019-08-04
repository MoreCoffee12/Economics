#' Computes the return for a portfolio
#'
#' @param string.portfolio.in.in string of the portfolio name
#' @param df.data.in data frame with the individual ticker values
#' @param df.symbols.in data frame with the metadata for the ticker symbols
#'
#' @return Modified data frame with the portfolio return
#'
#' @export
#'
#' @examples
pfUpdateReturn <-
  function(string.portfolio.in.in,
           df.data.in,
           df.symbols.in) {
    # Add the column to the dataframe and initialize to zero
    df.data.in[, string.portfolio.in.in] <- 0

    # Iterate through each of the columns and calculate the total return
    # for the portfolio
    for (col_name in names(df.data.in))
    {
      # Act only if the data is numeric
      if (is.numeric(df.data.in[, col_name]))
      {
        # Split the name ("USGFG.Close" is "USGFG" and "Close")
        lstSyms <- lstSymSplit(col_name)

        # Only if there is two terms
        if (length(lstSyms) > 1) {
          if (lstSyms[2] == 'Close_Norm') {
            dPercent <-
              df.symbols.in[df.symbols.in$string.symbol == lstSyms[1], string.portfolio.in.in]
            if (length(dPercent) > 0) {
              df.data.in[, string.portfolio.in.in] <-
                df.data.in[, string.portfolio.in.in] + (df.data.in[, col_name] * dPercent)
            }
          }

        }

      }
    }

    return(df.data.in)

  }

strSymOnly <- function(datay) {
  return(lstSymSplit(datay)[1])
}

lstSymSplit <- function(datay) {
  lstSyms <- unlist(strsplit(datay, "\\."))
  return(lstSyms)
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

#' Define the generic single plane plot function
#'
#' @param datadf Data frame with time series
#' @param datax Column for the x-axis. Almost always a date
#' @param datay Column for the y-axis
#' @param titlelabel Label for the plot
#' @param xlabel x-axis label
#' @param ylabel y-axis label
#' @param xlim Limits for the x-axis
#' @param ylim Limits for the y-axis
#' @param b.legend Boolean, set to true to show the legend
#'
#' @return
#' @export
#'
#' @examples
plotSingle <-
  function(datadf,
           datax,
           datay,
           titlelabel,
           xlabel,
           ylabel,
           xlim,
           ylim,
           b.legend) {
    # The palette with black:
    cbbPalette <- getPaletteBlack()

    # The palette with grey:
    cbPalette <- getPaletteGrey()

    myplot <- ggplot() +
      theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
      theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
      theme(panel.grid.major.x = element_blank()) +
      theme(panel.grid.major.y = element_line(colour = "grey", size = 0.5)) +
      theme(panel.grid.minor.y = element_line(
        colour = "grey",
        size = 0.25,
        linetype = "dotted"
      )) +
      geom_line(
        data = datadf,
        aes_string(
          x = datax,
          y = datay,
          colour = factor(datay)
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
      scale_fill_continuous(name = "V") +
      #geom_smooth(method = "lm") +
      ggtitle(titlelabel) +
      labs(x = xlabel, y = ylabel) +
      scale_x_date(limits = xlim) +
      scale_y_continuous(limits = ylim) +
      if (b.legend) {
        theme(legend.position = "top")
      } else{
        theme(legend.position = "none")
      }



    return(myplot)
  }

# ------------------------------------------------------------------------------
# Define the function for calculating year over year growth.
# ------------------------------------------------------------------------------
CalcYoY <- function (datadf, strCol, iPeriods) {
  Nrow <- nrow(datadf)
  GrowthRateYoY <- rep(0, Nrow)
  GrowthRateYoY[(iPeriods + 1):Nrow] <-
    diff(as.matrix(datadf[[strCol]]), lag = iPeriods)
  GrowthRateYoY <- (GrowthRateYoY / datadf[[strCol]])
  return(GrowthRateYoY)
}

# Define return using log
CalcLogRet <- function(datadf, strCol, iPeriods) {
  Nrow <- nrow(datadf)
  GrowthRate <- rep(0, Nrow)
  GrowthRate[(iPeriods + 1):Nrow] <-
    diff(log(datadf[[strCol]]), lag = iPeriods)
  return(GrowthRate)
}


#' Small helper function to get the symbol description
#'
#' @param datay Column name (corresponds to the symbol)
#'
#' @return String with the description
#' @export
#'
#' @examples
getPlotTitle <- function(datay) {
  strTitle <-
    paste(datay, " | ", dfSyms[grep(paste("^", datay, "$", sep = ""), dfSyms$Symbol), ]$Desc)
  return(strTitle)
}

getPlotYLabel <- function(datay) {
  strY <-
    dfSyms[grep(paste("^", datay, "$", sep = ""), dfSyms$Symbol), ]$yLabel
  return(strY)
}

#' Plot a single series bench mark
#'
#' @param datay Column with data to be plotted against the benchmark
#' @param ylim Vertical plotting limits
#' @param df.symbols Data frame with the symbol information
#' @param dfData Data frame with the financial series
#' @param string.analysis.start Starting data for the analysis
#'
#' @return
#' @export
#'
#' @examples
plotSingleBench <-
  function(datay,
           ylim,
           df.symbols,
           dfData,
           string.analysis.start) {
    # The palette with black:
    cbbPalette <- getPaletteBlack()

    if (grep('.', datay) > 0) {
      strSym <- strSymOnly(datay)
      strTitle <-
        paste(datay, " | ", df.symbols[grep(strSym, df.symbols$string.symbol), ]$string.description)
      strYLabel <-
        df.symbols[grep(strSym, df.symbols$Symbol), ]$string.label.y
    } else{
      strTitle <-
        paste(datay, " | ", df.symbols[grep(datay, df.symbols$string.symbol), ]$string.description)
      strYLabel <-
        df.symbols[grep(datay, df.symbols$Symbol), ]$string.label.y
    }
    dataBench <- "GSPC.Close_Norm"
    datax <- "date"
    my.plot <- plotSingle(dfData,
                          datax,
                          datay,
                          strTitle,
                          "Date",
                          strYLabel,
                          c(as.Date(string.analysis.start), Sys.Date()),
                          ylim,
                          TRUE)
    # Label the end point for the data being compared to the benchmark
    d.y.value <- tail(dfData[[datay]], 1)
    my.plot <-
      my.plot + annotate(
        geom = "point",
        x = tail(dfData$date, 1),
        y = d.y.value,
        color = cbbPalette[2],
        size = 1.75
      )
    my.plot <-
      my.plot + annotate(
        geom = "text",
        x = tail(dfData$date, 1),
        y = d.y.value,
        label = format(d.y.value, digits = 3, nsmall = 2),
        vjust = 0.5,
        hjust = -0.1,
        color = cbbPalette[2]
      )

    my.plot <-
      my.plot + geom_line(
        data = dfData,
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

plotSingleQuick <- function(datay, ylim) {
  if (grep('.', datay) > 0) {
    strSym <- strSymOnly(datay)
    strTitle <-
      paste(datay, " | ", dfSyms[grep(strSym, dfSyms$Symbol), ]$Desc)
    strYLabel <- dfSyms[grep(strSym, dfSyms$Symbol), ]$yLabel
  } else{
    strTitle <-
      paste(datay, " | ", dfSyms[grep(datay, dfSyms$Symbol), ]$Desc)
    strYLabel <- dfSyms[grep(datay, dfSyms$Symbol), ]$yLabel
  }
  datax <- "date"
  myPlot <- plotSingle(dfData,
                       datax,
                       datay,
                       strTitle,
                       "Date",
                       strYLabel,
                       c(as.Date(strDateStart), Sys.Date()),
                       ylim,
                       TRUE)

  return(myPlot)


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

#' For a given portfolio, plot the correlation for the symbols
#'
#' @param string.portfolio.in Name of the portfolio to plot
#' @param df.data.in Data frame with the time history for the stock symbols
#' @param df.symbols.in Data frame with meta data for the stock symbols
#'
#' @return
#' @export
#'
#' @examples
plotCorr <-
  function(string.portfolio.in,
           df.data.in,
           df.symbols.in) {
    lstActiveSyms <-
      paste(df.symbols.in[df.symbols.in[, string.portfolio.in] > 0, ]$string.symbol, '.Close_Norm', sep = "")


    # Correlation for the entire data set
    training.cor <- df.data.in[, lstActiveSyms]
    rcorr.data <- rcorr(as.matrix(training.cor), type = "pearson")
    #print(rcorr.data)

    corrplot(
      cor(training.cor),
      type = "upper",
      order = "original",
      tl.col = "black",
      tl.srt = 45,
      title = "All data"
    )
  }
