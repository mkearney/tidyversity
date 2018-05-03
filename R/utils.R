#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

is_ols <- function(x) {
  grepl("^ols$|ordinary\\s?least\\s?squares", x, ignore.case = TRUE)
}

is_logistic <- function(x) {
  grepl("^log$|logistic|^binomial$", x, ignore.case = TRUE)
}

is_poisson <- function(x) {
  grepl("^pois$|poisson", x, ignore.case = TRUE)
}

is_negbinom <- function(x) {
  grepl("^negbinom$|negative\\s?binomial", x, ignore.case = TRUE)
}
