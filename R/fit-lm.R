
ols_fit <- function(m) {
  s <- summary.lm(m)
  ## f and its p value
  f <- s$fstatistic[1]
  fp <- do.call("pf", as.list(c(unname(s$fstatistic), lower.tail = FALSE)))
  ## root mean square error
  rmse <- rmse(m)
  ## deviance
  #ll <- -2 * logLik(m)
  #lln <- as.integer(attr(ll, "df")
  # AIC/BIC
  aic <- AIC(m)
  bic <- BIC(m)
  ## stat name and estimate
  fit_statistic <- c("F", "R^2", "Adj R^2", "RMSE", "AIC", "BIC")
  estimate <- c(f, s$r.squared, s$adj.r.squared, rmse, aic, bic)
  ## degrees of freedom
  df <- rep(NA_integer_, length(fit_statistic))
  df[match(fit_statistic[c(1)], fit_statistic)] <- c(as.integer(s$fstatistic[2]))
  n <- nobs(m)
  ## p values
  p.value <- rep(NA_real_, length(fit_statistic))
  p.value[match(c("F"), fit_statistic)] <- fp
  ## stars
  stars <- make_stars(p.value)
  ## return data frame
  tibble::data_frame(fit_stat = fit_statistic, n, df,
    estimate, p.value, stars)
}

rmse <- function(m) {
  x <- unname(m$residuals)
  n <- nobs(m)
  p <- length(variable.names(m))
  x <- (1 / (n - p)) * sum(x^2)
  sqrt(x)
}
