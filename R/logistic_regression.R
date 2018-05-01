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
  s <- summary(m)
  chisq <- (s$null.deviance - s$deviance)
  df <- s$df.null - s$df.residual
  p.value <- pchisq(chisq, df)
  csq <- paste0("χ2(", df, ")")
  fit_statistics <- data.frame(
    statistic = c(csq, "AIC", "BIC", "Nagelkerke R^2"),
    estimate = c(chisq, s$aic, BIC(m), nagelkerke(m)),
    p.value = c(p.value, NA_real_, NA_real_, NA_real_),
    stringsAsFactors = FALSE,
    check.rows = FALSE,
    row.names = NULL,
    check.names = FALSE
  )
  model_estimates <- broom::tidy(m)
  model_estimates <- add_stars(model_estimates)
  list(fit = fit_statistics,
    coef = model_estimates)
}
