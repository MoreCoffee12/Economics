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
#' 
#' @note Updated to use the adjusted return rather than the close, 1 Jan 2021
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
        # Split the name ("USGFG.Adjusted" is "USGFG" and "Adjusted")
        lstSyms <- lstSymSplit(col_name)

        # Only if there is two terms
        if (length(lstSyms) > 1) {
          if (lstSyms[2] == 'Adjusted_Norm') {
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


