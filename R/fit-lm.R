ols_fit <- function(m) {
  s <- summary.lm(m)
  fp <- pf(s$fstatistic[1L],
    s$fstatistic[2L],
    s$fstatistic[3L],
    lower.tail = FALSE)
  rmse <- rmse(m)
  ll <- -2 * logLik(m)
  aic <- AIC(m)
  bic <- BIC(m)
  ## stat name and estimate
  fit_statistic <- c("F", "R^2", "Adj R^2", "RMSE", "χ2", "AIC", "BIC")
  estimate <- c(s$fstatistic[1], s$r.squared, s$adj.r.squared, rmse, ll, aic, bic)
  ## degrees of freedom and n
  df <- rep(NA_integer_, length(fit_statistic))
  df[match(fit_statistic[1:2], fit_statistic)] <- c(
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
