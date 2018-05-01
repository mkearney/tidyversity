nagelkerke <- function(m) UseMethod("nagelkerke")

#' @details Equation taken from the following study:
#' Nagelkerke, N. (1991). A Note on a General Definition of the Coefficient of Determination. Biometrika, 78(3), 691-692. doi:10.2307/2337038
nagelkerke.glm <- function(m) {
  s <- summary(m)
  ll0 <- -s$null.deviance / 2
  ll1 <- -s$deviance / 2
  n <- nobs(m)
  1 - exp((-(2/n) * (ll1 - ll0)))
}
