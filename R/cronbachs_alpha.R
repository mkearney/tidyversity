


#' Estimate Cronbach's alpha coefficient of reliability
#'
#' Returns reliability estimate for a set of variables
#'
#' @param data Name of the data frame containing the variables of interest.
#' @param ... Names of variables (unquoted) on which to conduct analysis.
#' @return Numeric estimate of Cronbach's alpha (0-1 scale)
#' @examples
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


socdom <- readRDS("~/Dropbox/socdom.csv")


library(tidyverse)

d <- select(socdom, follow_trump:img2_hrc_15, img1_djt_1:img2_djt_15, pinterest_1:pie_4, vote_2016:hhinc, lat = LocationLatitude, lng = LocationLongitude, state, county)

all_numeric <- function(x) {
  x[1:ncol(x)] <- lapply(x, coerce_numeric)
  #tst <- vapply(x, is.numeric, FUN.VALUE = logical(1), USE.NAMES = FALSE)
  x
}
coerce_numeric <- function(x) UseMethod("coerce_numeric")

coerce_numeric.default <- function(x) {
  as.numeric(x)
}

coerce_numeric.character <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) {
    stop("You've included a character (textual) variable. This function expected only numeric, integer, or factor variables.",
      call. = FALSE)
  }
  x
}



eigen_values <- function(x) {
  x <- all_numeric(x)
  e <- eigen(cor(x, use = "pairwise"), only.values = TRUE, symmetric = TRUE)
  e <- tibble::data_frame(
    p = seq_along(e$values),
    eigenvalue = e$values,
    `>1.0` = e$values > 1)
  n <- sum(e[[3]])
  cat(paste0("There were ", n, " eigenvalues greater than 1.0."), fill = TRUE)
  p <- ggplot(e, aes(x = p, y = eigenvalue)) +
    geom_point(size = 2) + geom_line() +
    geom_hline(yintercept = 1.0, colour = "red", linetype = "dashed") +
    theme(legend.position = "none") +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, nrow(e))) +
    scale_y_continuous(breaks = seq(0, max(e$eigenvalue)))
  p
}

eigen_values(select(d, ambiv_sexism_1:ambiv_sexism_22))

psych::fa(select(d, ambiv_sexism_1:ambiv_sexism_22), nfactors = 3)


ambses <- select(d, ambiv_sexism_2, ambiv_sexism_5, ambiv_sexism_10,
  ambiv_sexism_11, ambiv_sexism_14, ambiv_sexism_16)

cronbachs_alpha(ambses)

