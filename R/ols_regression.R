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
  ##f <- as.character(rlang::enquo(model))
  m <- lm(model, data = data)
  sweep(m)
}

ols_fit <- function(m) {
  s <- summary(m)
  fp <- pf(s$fstatistic[1L],
    s$fstatistic[2L],
    s$fstatistic[3L],
    lower.tail = FALSE)
  rmse <- rmse(m)
  ll <- -2 * logLik(m)
  aic <- AIC(m)
  bic <- BIC(m)
  ## stat name and estimate
  fit_statistic <- c("F", "R^2", "Adj R^2", "RMSE", "-2*LL", "AIC", "BIC")
  estimate <- c(s$fstatistic[1], s$r.squared, s$adj.r.squared, rmse, ll, aic, bic)
  ## degrees of freedom and n
  df <- rep(NA_integer_, length(fit_statistic))
  df[match(c("F", "-2*LL"), fit_statistic)] <- c(
    as.integer(s$fstatistic[2]), as.integer(attr(ll, "df")))
  n <- nobs(m)
  ## p values
  p.value <- rep(NA_real_, length(fit_statistic))
  p.value[match(c("F"), fit_statistic)] <- round(fp, 5)
  ## stars
  stars <- make_stars(p.value)
  ## return data frame
  tibble::data_frame(fit_statistic, n, df, estimate, p.value, stars)
}

rmse <- function(m) {
  x <- unname(m$residuals)
  n <- nobs(m)
  p <- length(variable.names(m))
  x <- (1 / (n - p)) * sum(x^2)
  sqrt(x)
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
