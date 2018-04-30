#' Ordinary least squares (OLS) regression
#'
#' Conducts regression analysis to model outcome variable using OLS
#'
#' @param data Data frame containing variables of interest.
#' @param model Model formula to be estimated.
#' @param ... Other arguments passed to lm() function.
#' @return A list object containing a "fit" and "ceof" data frames.
#' @examples
#'
#' ## predict mpg using weight and cylinders
#' ols_regression(datasets::mtcars, mpg ~ wt + cyl)
#'
#' @export
ols_regression <- function(data, model, ...) {
  f <- as.character(rlang::enquo(model))
  m <- lm(model, data = data)
  fit_statistics <- ols_fit(m)
  model_estimates <- broom::tidy(m)
  model_estimates <- add_stars(model_estimates)
  cat(paste0("## Model call:\n", f[2], "\n"), fill = TRUE)
  cat("## Model estimates:", fill = TRUE)
  list(fit = fit_statistics,
    coef = model_estimates)
}

ols_fit <- function(m) {
  s <- summary(m)
  a <- unname(s$fstatistic)
  fp <- pf(s$fstatistic[1L],
    s$fstatistic[2L],
    s$fstatistic[3L],
    lower.tail = FALSE)
  ll <- logLik(m)
  aic <- AIC(m)
  bic <- BIC(m)
  df <- data.frame(
    fit_statistic = c("F", "R^2", "Adj R^2", "RMSE", "Log-likelihood", "AIC", "BIC"),
    n = nobs(m),
    df = c(a[2], NA_integer_, NA_integer_, NA_integer_, attr(ll, "df"),
      NA_integer_, NA_integer_),
    estimate = c(a[1], s$r.squared, s$adj.r.squared, s$sigma, ll, aic, bic),
    p.value = c(fp, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
    stringsAsFactors = FALSE,
    check.rows = FALSE,
    row.names = NULL,
    check.names = FALSE
  )
  add_stars(df)
}



build_model <- function(data, response, preds) {
  f <- paste0(response, " ~ ", paste(preds, collapse = " + "))
  lm(formula(f), data = data)
}
rsquared <- function(m) ols_fit(m)$estimate[[2]]
get_r2 <- function(data, response, preds) {
  m0 <- build_model(data, response, preds)
  r2_all <- rsquared(m0)
  r2 <- double(length(preds))
  for (i in seq_along(preds)) {
    r2[i] <- rsquared(build_model(data, response, preds[-i]))
    r2[i] <- r2_all - r2[i]
  }
  r2
}
get_preds <- function(f) strsplit(gsub("\\s+", "", as.character(f)[3]), "\\+")[[1]]
get_response <- function(f) gsub("\\s+", "", as.character(f)[2])
