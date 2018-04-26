


#' Estimate Cronbach's alpha coefficient of reliability
#'
#' Returns reliability estimate for a set of variables
#'
#' @param data
## x = data frame
## vars = character vector of variable names of interest
cronbachs_alpha <- function(data, ...) UseMethod("cronbachs_alpha")

## method for when x is a data frame
cronbachs_alpha.data.frame <- function(data, ...) {
  vars <- tidyselect::vars_select(names(data), ...)
  data <- psych::alpha(data[vars])
  data$total$raw_alpha
}

## method for when data is a list of data frames
cronbachs_alpha.list <- function(data, ...) {
  sapply(data, cronbachs_alpha, ...)
}


## validate class of data (make sure it's a data frame or list of data frames)
cronbachs_alpha.default <- function(data, ...) {
  stop("data or one of the elements in data is not a data frame!", call. = FALSE)
}

## run on a single data
cronbachs_alpha(df1, x, y, z)

## run on a list of data frames
list_of_dfs <- list(df1, df2, df3, df4)
cronbachs_alpha(list_of_dfs, x, y, z)

## return an error if an element of list is not a data frame
cronbachs_alpha(c(list_of_dfs, rnorm(10)), c("x", "y", "z"))

## return an error if input is not list or data frame
cronbachs_alpha(rnorm(20))
