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
#' polcom %>%
#'   dplyr::mutate(sex = ifelse(sex == 1, "Male", "Female"),
#'   vote_choice = dplyr::case_when(
#'     vote_2016_choice == 1 ~ "Clinton",
#'     vote_2016_choice == 2 ~ "Trump",
#'     TRUE ~ "Other")) %>%
#'   tidy_anova(pp_ideology ~ sex * vote_choice) %>%
#'   tidy_summary()
#'
#' @export
tidy_anova <- function(data, model, ...) {
  aov(model, data = data, ...)
}
