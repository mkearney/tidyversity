


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
cronbachs_alpha.data.frame <- function(data, ...) {
  vars <- tidyselect::vars_select(names(data), ...)
  if (length(vars) > 0) {
    data <- data[vars]
  }
  data <- all_numeric(data)
  a <- psych::alpha(data)
  a$total$raw_alpha
}

## validate class of data (make sure it's a data frame or list of data frames)
cronbachs_alpha.default <- function(data, ...) {
  stop("data is not a data frame!", call. = FALSE)
}

