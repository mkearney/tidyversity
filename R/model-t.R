
#' T-test
#'
#' Conducts a t-test, comparing two groups along single variable
#'
#' @param data Data frame with grouping and variable of interest.
#' @param model The model specified as the measured variable predicted by
#'   group.
#' @examples
#'
#' ## compre pp_ideology b/w those who follow and don't follow trump
#' polcom %>%
#'   tidy_ttest(pp_ideology ~ follow_trump) %>%
#'   tidy_summary()
#'
#' @return A tidy summary with fit and coef tibbles
#' @export
tidy_ttest <- function(data, model) {
  m <- t.test(model, data)
  attr(m, "tidycall") <- store_tidycall(c(nrow(data), 2L), model)
  m
}
