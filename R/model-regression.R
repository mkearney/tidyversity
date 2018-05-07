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
#' @param robust Logical indicating whether to estimate a robust model. This
#'   is available for all models but negative binomial.
#' @param ... Other arguments passed to modeling function.
#' @return A model object
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
  } else if (is_log(type)) {
    call <- "logistic_regression"
  } else if (is_qlog(type)) {
    call <- "quasilogistic_regression"
  } else if (is_pois(type)) {
    call <- "poisson_regression"
  } else if (is_qpois(type)) {
    call <- "quasipoisson_regression"
  }else if (is_negbin(type)) {
    call <- "negbinom_regression"
  } else {
    stop("cannot recognized type", call. = FALSE)
  }
  ## build args
  args <- list(model, data = data, robust = robust, ...)
  ## apply call to args
  m <- do.call(call, args)
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
    e <- rlang::expr(MASS::rlm(!!model, data, ...))
  } else {
    e <- rlang::expr(lm(!!model, data = data, ...))
  }
  ## estimate model
  m <- eval(e)
  ## store info as tidycall attribute
  attr(m, "tidycall") <- store_tidycall(m, e)
  ## return model object
  m
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
    e <- rlang::expr(robust::glmRob(!!model, data = data, family = binomial))
  } else {
    e <- rlang::expr(glm(!!model, data = data, family = binomial))
  }
  ## estimate model
  m <- eval(e)
  ## store info as tidycall attribute
  attr(m, "tidycall") <- store_tidycall(m, e)
  ## return model object
  m
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
quasilogistic_regression <- function(data, model, robust = FALSE, ...) {
  if (robust) {
    e <- rlang::expr(robust::glmRob(!!model, data = data, family = quasibinomial))
  } else {
    e <- rlang::expr(glm(!!model, data = data, family = quasibinomial))
  }
  ## estimate model
  m <- eval(e)
  ## store info as tidycall attribute
  attr(m, "tidycall") <- store_tidycall(m, e)
  ## return model object
  m
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
    e <- rlang::expr(robust::glmRob(!!model, data = data, family = poisson))
  } else {
    e <- rlang::expr(glm(!!model, data = data, family = poisson))
  }
  ## estimate model
  m <- eval(e)
  ## store info as tidycall attribute
  attr(m, "tidycall") <- store_tidycall(m, e)
  ## return model object
  m
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
quasipoisson_regression <- function(data, model, robust = FALSE, ...) {
  if (robust) {
    e <- rlang::expr(robust::glmRob(!!model, data = data, family = quasipoisson))
  } else {
    e <- rlang::expr(glm(!!model, data = data, family = quasipoisson))
  }
  ## estimate model
  m <- eval(e)
  ## store info as tidycall attribute
  attr(m, "tidycall") <- store_tidycall(m, e)
  ## return model object
  m
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
  if (robust) {
    stop(paste0("Robust is not currently available for negative binomial models.\n",
      "       If you know of a package that offers such a model, please file an\n",
      "       issue at https://github.com/mkearney/tidyversity/issues. Otherwise,\n",
      "       you can try the function found at the following link---though the\n",
      "       output is rather limited: https://github.com/williamaeberhard/glmrob.nb"),
      call. = FALSE)
  }
  ## capture model expression
  e <- rlang::expr(MASS::glm.nb(!!model, data = data, ...))
  ## estimate model
  m <- eval(e)
  ## store info as tidycall attribute
  attr(m, "tidycall") <- store_tidycall(m, e)
  ## return model object
  m
}
