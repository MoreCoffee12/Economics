#-----------------------df.symbols utilities------------------------------------
#' Append a single metadata row to df.symbols
#'
#' Append one row to `df.symbols` without manually constructing a matching
#' `data.frame()` each time. Validates inputs, fills sensible defaults, computes
#' safe names when requested, and aligns columns and types automatically.
#'
#' @param df.symbols A data.frame to append to (may be empty).
#' @param fields A named list or a 1-row data.frame containing values for the
#'   new row (e.g., `list(string.symbol="RSALESAGG", string.source="Calc", ...)`).
#'   All supplied values must be length-1; use `NA` for unknowns.
#' @param compute_safe Logical; if `TRUE` and `string.symbol_safe` or
#'   `string.object_name` are absent but `string.symbol` is provided, they are
#'   computed via `safe_symbol_name()`. Default `TRUE`.
#'
#' @returns The updated `df.symbols` with the new row appended.
#' @export
#'
#' @examples
#' df.symbols <- data.frame(stringsAsFactors = FALSE)
#' df.symbols <- symbols_append_row(
#'   df.symbols,
#'   list(
#'     string.symbol       = "RSALESAGG",
#'     string.source       = "Calc",
#'     string.description  = "Real Retail and Food Services Sales\n(RRSFS and RSALES)",
#'     string.label.y      = "Millions of Dollars",
#'     float.expense.ratio = -1.0,
#'     date.series.start   = as.Date("2000-01-01"),
#'     date.series.end     = as.Date("2025-08-01"),
#'     status              = "ok",
#'     nrows               = 0L
#'   )
#' )
symbols_append_row <- function(df.symbols, fields, compute_safe = TRUE) {
  # ---- helpers --------------------------------------------------------------
  stopifnot(is.data.frame(df.symbols) || is.null(df.symbols))
  
  na_like <- function(x) x[NA_integer_]  # NA of same type as x
  
  coerce1 <- function(x, type) {
    if (length(x) == 0L) return(switch(type,
                                       character = NA_character_,
                                       numeric   = as.numeric(NA),
                                       double    = as.numeric(NA),
                                       integer   = NA_integer_,
                                       logical   = NA,
                                       Date      = as.Date(NA),
                                       x
    ))
    switch(type,
           character = as.character(x)[1],
           numeric   = as.numeric(x)[1],
           double    = as.numeric(x)[1],
           integer   = as.integer(x)[1],
           logical   = as.logical(x)[1],
           Date      = as.Date(x)[1],
           x[1]
    )
  }
  
  # canonical schema (types) for known columns; others will pass through
  schema <- c(
    string.symbol       = "character",
    string.source       = "character",
    string.description  = "character",
    string.label.y      = "character",
    float.expense.ratio = "numeric",
    date.series.start   = "Date",
    date.series.end     = "Date",
    string.symbol_safe  = "character",
    string.object_name  = "character",
    status              = "character",
    error               = "character",
    nrows               = "integer",
    first_date          = "Date",
    last_date           = "Date"
  )
  
  # ---- normalize inputs -----------------------------------------------------
  if (is.data.frame(fields)) {
    if (nrow(fields) != 1L)
      stop("`fields` data.frame must have exactly 1 row.")
    fields <- as.list(fields[1, , drop = TRUE])
  } else if (!is.list(fields)) {
    stop("`fields` must be a named list or a 1-row data.frame.")
  }
  
  if (is.null(names(fields)) || any(names(fields) == ""))
    stop("All elements of `fields` must be named.")
  
  too_long <- vapply(fields, length, integer(1)) > 1L
  if (any(too_long)) {
    stop("All values in `fields` must be length-1. Offenders: ",
         paste(names(fields)[too_long], collapse = ", "))
  }
  
  # compute safe names if requested
  if (isTRUE(compute_safe) && !is.null(fields$string.symbol)) {
    if (is.null(fields$string.symbol_safe)) {
      fields$string.symbol_safe <- safe_symbol_name(fields$string.symbol)
    }
    if (is.null(fields$string.object_name)) {
      fields$string.object_name <- safe_symbol_name(fields$string.symbol)
    }
  }
  
  # defaults derived from other fields
  if (is.null(fields$first_date) && !is.null(fields$date.series.start)) {
    fields$first_date <- fields$date.series.start
  }
  if (is.null(fields$last_date) && !is.null(fields$date.series.end)) {
    fields$last_date <- fields$date.series.end
  }
  if (is.null(fields$status)) fields$status <- "ok"
  if (is.null(fields$error))  fields$error  <- NA_character_
  if (is.null(fields$nrows))  fields$nrows  <- 0L
  
  # ---- build 1-row data.frame for the new record ----------------------------
  row_names <- union(names(schema), names(fields))
  row_list  <- setNames(vector("list", length(row_names)), row_names)
  
  for (nm in row_names) {
    if (!is.null(fields[[nm]])) {
      # coerce to schema type if known
      if (nm %in% names(schema)) {
        row_list[[nm]] <- coerce1(fields[[nm]], schema[[nm]])
      } else {
        row_list[[nm]] <- fields[[nm]]
      }
    } else {
      # fill NA of schema type (or character if unknown)
      if (nm %in% names(schema)) {
        row_list[[nm]] <- coerce1(NA, schema[[nm]])
      } else {
        row_list[[nm]] <- NA_character_
      }
    }
  }
  
  new_row <- as.data.frame(row_list, stringsAsFactors = FALSE, check.names = FALSE)
  
  # ---- align columns & types, then append -----------------------------------
  if (is.null(df.symbols)) df.symbols <- data.frame(stringsAsFactors = FALSE)
  
  cols_union <- union(names(df.symbols), names(new_row))
  
  # add missing cols to df.symbols with NA of the *row's* type
  for (nm in setdiff(cols_union, names(df.symbols))) {
    df.symbols[[nm]] <- rep(na_like(new_row[[nm]]), nrow(df.symbols))
  }
  # add missing cols to new_row with NA of the *df*'s type (if any)
  for (nm in setdiff(cols_union, names(new_row))) {
    new_row[[nm]] <- na_like(df.symbols[[nm]])
  }
  
  df.symbols <- df.symbols[, cols_union, drop = FALSE]
  new_row    <- new_row[,    cols_union, drop = FALSE]
  
  rbind(df.symbols, new_row)
}


#-------- Check that the columns exist in the data frame-----------------------
#' Check if data frame contains required columns
#'
#' This helper function verifies whether all specified column names
#' are present in a data frame. Returns \code{TRUE} if all columns exist,
#' otherwise \code{FALSE}.
#'
#' @param df A data frame to check.
#' @param cols A character vector of column names to verify.
#'
#' @return A logical scalar: \code{TRUE} if all required columns exist,
#'   otherwise \code{FALSE}.
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:3, b = 4:6)
#' require_columns(df, c("a", "b"))    # TRUE
#' require_columns(df, c("a", "c"))    # FALSE
require_columns <- function(df, cols ) {
  miss <- setdiff(cols, names(df))
  if (length(miss)){
    FALSE
  }else{
    TRUE
  } 
}

