#' @export
glm_fit <- function(m) {
  s <- summary(m)
  devn <- s$df.residual
  devp <- pchisq(s$deviance, devn, lower.tail = FALSE)
  nulln <- s$df.null
  nullp <- pchisq(s$null.deviance, nulln, lower.tail = FALSE)
  chisq <- s$null.deviance - s$deviance
  chisqn <- nulln - devn
  chisqp <- pchisq(chisq, chisqn, lower.tail = FALSE)
  aic <- AIC(m)
  bic <- BIC(m)
  rmse <- rmse(m)
  r2nag <- nagelkerke(m)
  #r2cox <- coxsnell(m)
  r2mcf <- mcfadden(m)
  ##mcfadden.adj(m)
  ## names of fit statistics
  fit_statistic <- c("χ2","Δχ2", "Nagelkerke R^2",
    "McFadden R^2", "RMSE", "AIC", "BIC")
  ## estimates
  estimate <- c(s$deviance, chisq, r2nag, r2mcf, rmse, aic, bic)
  ## degrees of freedom
  df <- rep(NA_integer_, length(fit_statistic))
  df[match(fit_statistic[1:2], fit_statistic)] <- c(devn, chisqn)
  ## p values
  p.value <- rep(NA_real_, length(fit_statistic))
  p.value[match(fit_statistic[1:2], fit_statistic)] <- c(devp, chisqp)
  ## number of obs
  n <- nobs(m)
  ## stars
  stars <- make_stars(p.value)
  ## return data frame
  tibble::data_frame(fit_stat = fit_statistic, n, df,
    estimate, p.value, stars)
}

#' nagelkerke r squared
#'
#' Estimate R^2 approximation for model object
#'
#' @param m A GLM model object.
#' @return R^2 estimate.
#' @details Equation taken from the following study:
#' Nagelkerke, N. (1991). A Note on a General Definition of the Coefficient of Determination. Biometrika, 78(3), 691-692. doi:10.2307/2337038
#' @export
nagelkerke <- function(m) UseMethod("nagelkerke")

#' @export
nagelkerke.glm <- function(m) {
  s <- summary(m)
  ll0 <- -s$null.deviance / 2
  ll1 <- -s$deviance / 2
  n <- nobs(m)
  1 - exp((-(2/n) * (ll1 - ll0)))
}

mcfadden <- function(m) {
  s <- summary(m)
  ll0 <- -s$null.deviance / 2
  ll1 <- -s$deviance / 2
  1 - ll1 / ll0
}

mcfadden.adj <- function(m) {
  s <- summary(m)
  ll0 <- -s$null.deviance / 2
  ll1 <- -s$deviance / 2
  1 - (ll1 - ncol(m$model) - 1) / ll0
}

coxsnell <- function(m) {
  s <- summary(m)
  ll0 <- -s$null.deviance / 2
  ll1 <- -s$deviance / 2
  n <- nobs(m)
  1 - ((ll0 / ll1)^(2 / n))
}
