#' Analysis of variance
#'
#' Conducts analysis of variance (ANOVA)
#'
#' @param data Data frame with variables of interest to model
#' @param model Specification of the theorized model
#' @param type Not yet impelmented
#' @param robust Logical not yet implemented
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
tidy_anova <- function(data, model, type = NULL, robust = FALSE, ...) {
  if (robust) {
    stop("Sorry, robust has not yet been implemented of anova", call. = FALSE)
  }
  e <- rlang::expr(aov(!!model, data = data, ...))
  ## estimate model
  m <- eval(e)
  ## store info as tidycall attribute
  attr(m, "tidycall") <- store_tidycall(dim(model.frame(m)), e)
  ## return model object
  m
}

