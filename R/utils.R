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

#' Select columns in data frame
#'
#' @name select
#' @export
#' @importFrom dplyr select
NULL

#' Filter rows in data frame
#'
#' @name filter
#' @export
#' @importFrom dplyr filter
NULL

#' Group by variable in data frame
#'
#' @name group_by
#' @export
#' @importFrom dplyr group_by
NULL

#' Ungroup by variable in data frame
#'
#' @name ungroup
#' @export
#' @importFrom dplyr ungroup
NULL

#' Summarise data frame
#'
#' @name summarise
#' @export
#' @importFrom dplyr summarise
NULL

#' Create new variables in data frame
#'
#' @name mutate
#' @export
#' @importFrom dplyr mutate
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

is_poisson <- function(x) {
  grepl("^pois$|poisson", x, ignore.case = TRUE)
}

is_negbinom <- function(x) {
  grepl("^negbinom$|negative\\s?binomial", x, ignore.case = TRUE)
}
