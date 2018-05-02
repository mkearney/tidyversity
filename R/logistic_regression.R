#' Logistic regression for dichotomous outcomes
#'
#' Conducts logistic regression analysis to model binary outcome variable using
#'   a generalized (binomial with logit link) linear model
#'
#' @param data Data frame containing variables of interest.
#' @param model Model formula to be estimated.
#' @param ... Other arguments passed to lm() function.
#' @return A list object containing a "fit" and "ceof" data frames.
#' @examples
#'
#' ## predict mpg using weight and cylinders
#' logistic_regression(datasets::mtcars, am ~ disp + carb + mpg)
#'
#' @export
logistic_regression <- function(data, model, ...) {
  m <- glm(model, data = data, family = binomial)
  sweep(m)
}

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
  r2cox <- coxsnell(m)
  r2mcf <- mcfadden(m)
  ##mcfadden.adj(m)
  ## names of fit statistics
  fit_statistic <- c("χ2.full", "χ2.null","Δχ2", "Nagelkerke R^2",
    "Cox & Snell R^2", "McFadden R^2", "AIC", "BIC")
  ## estimates
  estimate <- c(s$deviance, s$null.deviance, chisq, r2nag, r2cox, r2mcf, aic, bic)
  ## degrees of freedom
  df <- rep(NA_integer_, length(fit_statistic))
  df[match(fit_statistic[1:3], fit_statistic)] <- c(devn, s$df.null, chisqn)
  ## p values
  p.value <- rep(NA_real_, length(fit_statistic))
  p.value[match(fit_statistic[1:3], fit_statistic)] <- c(devp, nullp, chisqp)
  ## number of obs
  n <- nobs(m)
  ## stars
  stars <- make_stars(p.value)
  ## return data frame
  tibble::data_frame(fit_statistic, n, df, estimate, p.value, stars)
}

