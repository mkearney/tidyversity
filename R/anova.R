#' Analysis of variance
#'
#' Conducts analysis of variance (ANOVA)
#'
#' @param data Data frame with variables of interest to model
#' @param model Specification of the theorized model
#' @param ... Other args passed to aov/lm
#' @return List with fit and coef elements.
#' @examples
#'
#' ## anova
#' ANOVA(polcom, pp_ideology ~ as.factor(sex) + age + pie_1)
#'
#' @export
ANOVA <- function(data, model, ...) {
  m <- aov(model, data = data, ...)
  f <- lm(model, data = data, ...)
  f <- ols_fit(f)
  coef <- broom::tidy(m)
  coef <- add_stars(coef)
  list(fit = f, coef = coef)
}
