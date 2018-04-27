


#' Estimate Cronbach's alpha coefficient of reliability
#'
#' Returns reliability estimate for a set of variables
#'
#' @param data Name of the data frame containing the variables of interest.
#' @param ... Names of variables (unquoted) on which to conduct analysis.
#' @return Numeric estimate of Cronbach's alpha (0-1 scale)
#' @examples
#'
#' ## estimate reliability of social media use items in polcom_survey data
#' cronbachs_alpha(polcom_survey, smuse1:smuse3)
#'
#' @export
cronbachs_alpha <- function(data, ...) UseMethod("cronbachs_alpha")

## method for when x is a data frame
#' @export
cronbachs_alpha.data.frame <- function(data, ...) {
  data <- tidyselector(data, ...)
  ## convert columns to numeric
  data <- all_numeric(data)
  a <- psych::alpha(data)
  item <- row.names(a$alpha.drop)
  #sprintf("all_%d_items", length(item))
  data.frame(
    items = c(paste(item[c(1, length(item))], collapse = ":"), paste0("-", item)),
    alpha = c(a$total$raw_alpha, a$alpha.drop[[1]]),
    alpha.std = c(a$total$std.alpha, a$alpha.drop[[2]]),
    check.rows = FALSE, row.names = NULL, check.names = FALSE)
}

## validate class of data (make sure it's a data frame or list of data frames)
#' @export
cronbachs_alpha.default <- function(data, ...) {
  stop("data is not a data frame!", call. = FALSE)
}

