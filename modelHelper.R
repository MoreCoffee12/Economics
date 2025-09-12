#' Extract (and optionally print) LM coefficients from a caret model
#'
#' Given a `caret::train()` fit with `method = "lm"`, this helper extracts the
#' linear model, returns the coefficients on the **standardized scale** used
#' during training, and—if centering/scaling was applied—converts them to the
#' **original data scale**:
#'
#' If predictors were centered/scaled as
#' \deqn{x_k^* = (x_k - \mu_k)/\sigma_k,}
#' then for \eqn{y = a + \sum_k b_k x_k^*} we have
#' \deqn{\beta_k = b_k / \sigma_k,\quad \alpha = a - \sum_k b_k\,\mu_k/\sigma_k.}
#'
#' @param fit A `caret::train` object trained with `method = "lm"`.
#' @param print_report Logical; if `TRUE`, prints a compact report including the
#'   `summary(lm)` and both sets of coefficients. Default `TRUE`.
#' @param digits Integer; number of digits to print for coefficients. Default `6`.
#'
#' @return A list (invisibly) with elements:
#' \itemize{
#'   \item `lm`: the underlying `lm` object (`fit$finalModel`)
#'   \item `coefs_scaled`: named numeric vector of intercept and betas on the
#'         standardized predictor scale
#'   \item `coefs_original`: named numeric vector of intercept and betas mapped
#'         back to the original predictor scale (identical to scaled if no
#'         center/scale pre-processing is present)
#'   \item `table`: a data.frame with `term`, `estimate_standardized`,
#'         `estimate_original`
#' }
#'
#' @examples
#' \dontrun{
#' fit <- caret::train(
#'   y ~ x1 + x2,
#'   data = df.train,
#'   method = "lm",
#'   preProcess = c("center", "scale")
#' )
#' res <- extract_lm_weights(fit)
#' res$table
#' }
#'
#' @export
extract_lm_weights <- function(fit, print_report = TRUE, digits = 6) {
  # ---- Validation ----------------------------------------------------------
  if (!inherits(fit, "train"))
    stop("`fit` must be a caret::train object.")
  if (!inherits(fit$finalModel, "lm"))
    stop("`fit$finalModel` is not an 'lm'. Did you train with method = 'lm'?")
  
  # ---- Pull lm + coefficients (standardized predictor space) --------------
  lm_mod <- fit$finalModel
  coefs_scaled <- stats::coef(lm_mod)  # (Intercept), then betas
  pred_names <- names(coefs_scaled)[-1]
  
  # ---- Attempt to unscale to original data units --------------------------
  # Default: assume no preProcess; original == scaled
  coefs_orig <- coefs_scaled
  
  pp <- fit$preProcess
  has_pp <- !is.null(pp) && !is.null(pp$mean) && !is.null(pp$std)
  
  if (has_pp && length(pred_names)) {
    mu <- pp$mean[pred_names]
    sd <- pp$std[pred_names]
    
    # Sanity checks on availability of means/stds for all predictors
    if (anyNA(mu) || anyNA(sd)) {
      miss_mu <- pred_names[is.na(mu)]
      miss_sd <- pred_names[is.na(sd)]
      warn_msg <- c()
      if (length(miss_mu)) warn_msg <- c(
        warn_msg, sprintf("missing means for: %s", paste(miss_mu, collapse = ", "))
      )
      if (length(miss_sd)) warn_msg <- c(
        warn_msg, sprintf("missing std devs for: %s", paste(miss_sd, collapse = ", "))
      )
      warning("Partial preProcess info; returning scaled coefficients only (",
              paste(warn_msg, collapse = "; "), ").")
    } else {
      intercept_scaled <- unname(coefs_scaled[[1]])
      betas_scaled     <- unname(coefs_scaled[-1])
      
      betas_orig     <- betas_scaled / sd
      intercept_orig <- intercept_scaled - sum(betas_scaled * (mu / sd))
      
      coefs_orig <- c("(Intercept)" = intercept_orig)
      coefs_orig <- c(coefs_orig, stats::setNames(betas_orig, pred_names))
    }
  }
  
  # ---- Tidy table ----------------------------------------------------------
  tbl <- data.frame(
    term = names(coefs_scaled),
    estimate_standardized = unname(coefs_scaled),
    estimate_original     = unname(coefs_orig[names(coefs_scaled)]),
    row.names = NULL,
    check.names = FALSE
  )
  
  # ---- Optional printing ---------------------------------------------------
  if (isTRUE(print_report)) {
    cat("\n== Linear Model (caret::train(method = 'lm')) ==\n")
    print(summary(lm_mod))
    
    cat("\n-- Weights on standardized predictors (as trained) --\n")
    print(round(coefs_scaled, digits))
    
    cat("\n-- Weights transformed to ORIGINAL data scale --\n")
    print(round(coefs_orig, digits))
  }
  
  invisible(list(
    lm = lm_mod,
    coefs_scaled = coefs_scaled,
    coefs_original = coefs_orig,
    table = tbl
  ))
}
