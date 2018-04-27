

#' Poisson regression modelingn for count data
#'
#' Conducts poisson regression (generalized linear models for count data)
#'
#' @param data Data frame with variables of interest to model
#' @param model Specification of the theorized model
#' @param ... Other args passed to glm
#' @return List with fit and coef elements.
#' @export
poisson_regression <- function(data, model, ...) {
  m <- glm(model, data = data, family = poisson)
  s <- summary(m)
  list(fit = s, coef = broom::tidy(m))
}
