#' Tidy regression analysis
#'
#' Conducts regression analysis to model outcome variable using OLS, logistic,
#' poisson, and negative-binomial models
#'
#' @param data Data frame containing variables in model
#' @param model Model formula to be estimated.
#' @param type Type of regression model to use. Available types include
#'   \code{"ols"}, \code{"logistic"}, \code{"poisson"}, \code{"negbinom"}. See
#'   \link{regression_types} for more information.
#' @param ... Other arguments passed to modeling function.
#' @return A list object containing a "fit", "ceof", and "data" data frames
#' @details In addition to being a wrapper function for \link[base]{lm},
#' \link[base]{glm}, and robust models via \link[MASS]{rlm} (for robust OLS)
#' and \link[robust]{glmRob} this function (a) ensures \code{data} arguments
#' appear in first position for better consistency and easier piping and (b)
#' stores information about the call
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
tidy_regression <- function(data, model, type = "ols", robust = FALSE, ...) {
  ## convert type to call
  if (is_ols(type)) {
    call <- "ols_regression"
  } else if (is_logistic(type)) {
    call <- "logistic_regression"
  } else if (is_quasibinom(type)) {
    call <- "quasi_logistic_regression"
  } else if (is_poisson(type)) {
    call <- "poisson_regression"
  } else if (is_quasipois(type)) {
    call <- "quasi_poisson_regression"
  }else if (is_negbinom(type)) {
    call <- "negbinom_regression"
  } else {
    stop("cannot recognized type", call. = FALSE)
  }
  ## build args
  args <- list(model, data = data, ...)
  if (!"robust" %in% args) {
    args$robust <- FALSE
  }
  ## apply call to args
  m <- do.call(call, args)
  ## store info as tidycall attribute
  attr(m, "tidycall") <- store_tidycall(m, model, type, robust)
  ## return model object
  m
}

#' Types of regression models
#'
#' @name regression_types
#' @details Available types of regression models.
NULL

#' Ordinary least squares (OLS) regression
#'
#' Conducts regression analysis to model outcome variable using OLS
#'
#' @inheritParams tidy_regression
#' @rdname regression_types
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
#' @inheritParams tidy_regression
#' @rdname regression_types
#' @export
logistic_regression <- function(data, model, robust = FALSE, ...) {
  if (robust) {
    robust::glmRob(model, data = data, family = binomial)
  } else {
    glm(model, data = data, family = binomial)
  }
}

#' Quasi-logistic regression for dichotomous-like outcomes
#'
#' Conducts logistic regression analysis to model approximations of binary
#' outcome variables (doesn't have to be 2 levels) using a generalized
#' (quasi-binomial with logit link) linear model
#'
#' @inheritParams tidy_regression
#' @rdname regression_types
#' @export
quasi_logistic_regression <- function(data, model, robust = FALSE, ...) {
  if (robust) {
    robust::glmRob(model, data = data, family = quasibinomial)
  } else {
    glm(model, data = data, family = quasibinomial)
  }
}

#' Poisson regression for modeling of count data
#'
#' Conducts poisson regression to model count outcome variables using a
#' generalized (poisson with logit link) linear model.
#'
#' @inheritParams tidy_regression
#' @rdname regression_types
#' @export
poisson_regression <- function(data, model, robust = FALSE, ...) {
  if (robust) {
    robust::glmRob(model, data = data, family = poisson)
  } else {
    glm(model, data = data, family = poisson)
  }
}

#' Quasi-poisson regression for count-like data
#'
#' Conducts poisson regression analysis to model approximations of count
#' outcome variables (doesn't have to be integers) using a generalized
#' (quasi-poisson with logit link) linear model
#'
#' @inheritParams tidy_regression
#' @rdname regression_types
#' @export
quasi_poisson_regression <- function(data, model, robust = FALSE, ...) {
  if (robust) {
    robust::glmRob(model, data = data, family = quasipoisson)
  } else {
    glm(model, data = data, family = quasipoisson)
  }
}


#' Negative binomial regression for modeling of overdispersed count data
#'
#' Conducts negative binomial regression (generalized linear models for
#' overdispersed count data)
#'
#' @inheritParams tidy_regression
#' @rdname regression_types
#' @export
negbinom_regression <- function(data, model, robust = FALSE, ...) {
  MASS::glm.nb(model, data = data, ...)
}
