
rmse <- function(m) {
  x <- unname(m$residuals)
  n <- length(x)
  p <- length(variable.names(m))
  x <- (1 / (n - p)) * sum(x^2)
  sqrt(x)
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
nagelkerke.default <- function(m) {
  s <- summary(m)
  ll0 <- -s$null.deviance / 2
  ll1 <- -s$deviance / 2
  n <- length(m$residuals)
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
  n <- length(m$residuals)
  1 - ((ll0 / ll1)^(2 / n))
}
