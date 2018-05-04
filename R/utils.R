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


#' Convert object to tibble
#'
#' @inheritParams tibble::as_tibble
#' @param validate Logical, indicating whether to validate data frame.
#'   Defaults to \code{FALSE} because validation, while probably offering more
#'   safety and consistency, also provides a noticeable slow down in
#'   performance.
#' @return A tibble, class \code{c("tbl_df", "tbl", "data.frame")}
#' @export
as_tbl <- function(x, ..., validate = FALSE) tibble::as_tibble(x, ..., validate = FALSE)

#' Create a tibble data frame
#'
#' @inheritParams tibble::tibble
#' @return A tibble, class \code{c("tbl_df", "tbl", "data.frame")}
#' @export
tbl_frame <- function(...) {
  xs <- rlang::quos(..., .named = TRUE)
  if (length(xs) == 1L) {
    x <- eval_tidy(xs[[1]])
    if (is.data.frame(x)) {
      return(as_tbl(x))
    }
  }
  as_tbl(tibble:::lst_quos(xs, expand = TRUE))
}

is_ols <- function(x) {
  grepl("^ols$|ordinary\\s?least\\s?squares", x, ignore.case = TRUE)
}

is_logistic <- function(x) {
  grepl("^log$|logistic|^binomial$", x, ignore.case = TRUE)
}

is_quasibinom <- function(x) {
  grepl("^quasi.?binom", x, ignore.case = TRUE)
}

is_poisson <- function(x) {
  grepl("^pois$|poisson", x, ignore.case = TRUE)
}

is_quasipois <- function(x) {
  grepl("^quasi.?pois", x, ignore.case = TRUE)
}

is_negbinom <- function(x) {
  grepl("^negbin$|^negbinom$|negative\\s?binomial", x, ignore.case = TRUE)
}
