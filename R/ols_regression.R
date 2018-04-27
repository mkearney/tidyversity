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
#' ols_regression(datasets::mtcars, mpg ~ wt + cyl)
#'
#' @export
ols_regression <- function(data, model, ...) {
  m <- lm(model, data = data)
  s <- summary(m)
  a <- unname(s$fstatistic)
  anm <- paste0("F(", a[2], ", ", a[3], ")")
  fp <- pf(s$fstatistic[1L],
    s$fstatistic[2L],
    s$fstatistic[3L],
    lower.tail = FALSE)
  fit_statistics <- data.frame(
    statistic = c(anm, "R^2", "Adj R^2"),
    estimate = c(a[1], s$r.squared, s$adj.r.squared),
    p.value = c(fp, NA_real_, NA_real_),
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

