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
#' m1 <- ols_regression(datasets::mtcars, mpg ~ wt + cyl)
#'
#' ## sweep and view summary
#' (s1 <- sweep(m1))
#' @export
ols_regression <- function(data, model, ...) {
  lm(model, data = data, ...)
}


#' Robust version of ordinary least squares (OLS) regression
#'
#' @inheritParams ols_regression
#' @export
#' @rdname ols_regression
ols_regression_robust <- function(data, model, ...) {
  MASS::rlm(model, data = data, ...)
}




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
#' ## predict am using disp, carb, and mpg
#' m1 <- logistic_regression(datasets::mtcars, am ~ disp + carb + mpg)
#'
#' ## sweep and view summary
#' (s1 <- sweep(m1))
#'
#' @export
logistic_regression <- function(data, model, ...) {
  glm(model, data = data, family = binomial)
}




#' Poisson regression for modeling of count data
#'
#' Conducts poisson regression (generalized linear models for count data)
#'
#' @param data Data frame with variables of interest to model
#' @param model Specification of the theorized model
#' @param ... Other args passed to glm
#' @return List with fit and coef elements.
#' @export
poisson_regression <- function(data, model, ...) {
  glm(model, data = data, family = poisson)
}

