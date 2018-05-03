#' Tidy regression analysis
#'
#' Conducts regression analysis to model outcome variable using OLS, logistic,
#' poisson, and negative-binomial models
#'
#' @param data Data frame containing variables in model
#' @param model Model formula to be estimated
#' @param ... Other arguments passed to modeling function
#' @return A list object containing a "fit", "ceof", and "data" data frames
#' @examples
#'
#' ## predict mpg using weight and cylinders
#' m_ols <- datasets::mtcars %>%
#'   tidy_regression(mpg ~ wt + cyl)
#'
#' ## sweep and view summary
#' (s_ols <- sweep(m_ols))
#'
#' ## logistic regression predict am using disp, carb, and mpg
#' m_logistic <- datasets::mtcars %>%
#'   tidy_regression(am ~ disp + carb + mpg, type = "logistic")
#'
#' ## sweep and view summary
#' (s_logistic <- sweep(m_logistic))
#'
#' ## poisson regression predict cyl using disp, carb, and mpg
#' m_poisson <- datasets::mtcars %>%
#'   tidy_regression(cyl ~ disp + carb + mpg, type = "poisson")
#'
#' ## sweep and view summary
#' (s_poisson <- sweep(m_poisson))
#'
#' @export
tidy_regression <- function(data, model, type = "ols", ...) {
  args <- list(model, data = data, ...)
  if (!"robust" %in% args) {
    args$robust <- FALSE
  }
  if (is_ols(type)) {
    call <- "ols_regression"
  } else if (is_logistic(type)) {
    call <- "logistic_regression"
  } else if (is_poisson(type)) {
    call <- "poisson_regression"
  } else if (is_negbinom(type)) {
    call <- "negbinom_regression"
  } else {
    stop("cannot recognized type", call. = FALSE)
  }
  do.call(call, args)
}

#' Ordinary least squares (OLS) regression
#'
#' Conducts regression analysis to model outcome variable using OLS
#'
#' @inheritParams regression
#' @rdname regression
#' @export
ols_regression <- function(data, model, robust = FALSE, ...) {
  if (robust) {
    MASS::rlm(model, data, ...)
  } else {
    lm(model, data = data, ...)
  }
}


#' Logistic regression for dichotomous outcomes
#'
#' Conducts logistic regression analysis to model binary outcome variable using
#'   a generalized (binomial with logit link) linear model
#'
#' @inheritParams regression
#' @rdname regression
#' @export
logistic_regression <- function(data, model, robust = FALSE, ...) {
  glm(model, data = data, family = binomial)
}


#' Negative binomial regression for modeling of overdispersed count data
#'
#' Conducts negative binomial regression (generalized linear models for
#' overdispersed count data)
#'
#' @inheritParams regression
#' @rdname regression
#' @export
poisson_regression <- function(data, model, robust = FALSE, ...) {
  glm(model, data = data, family = poisson)
}


#' Poisson regression for modeling of count data
#'
#' Conducts poisson regression (generalized linear models for count data)
#'
#' @inheritParams regression
#' @rdname regression
#' @export
poisson_regression <- function(data, model, robust = FALSE, ...) {
  glm(model, data = data, family = poisson)
}

