
#' Conduct factor analysis to estimate item loadings on each factor
#'
#' Provides estimates from a rotated factor analysis.
#'
#' @param data Data frame of interest
#' @param ... Variables (unquoted) of interest. If not provided, all variables
#'   in data will be used
#' @param n Number of retained factors, defaulting to number of factors
#'   suggested via parallel analysis.
#' @return Output of factor analysis.
#' @examples
#'
#' ## conduct parallel analysis of polcom news use items
#' factor_analysis(polcom, news_1:news_6)
#'
#' @export
#' @rdname factor_analysis
factor_analysis <- function(data, ..., n = NULL) {
  data <- tidyselector(data, ...)
  if (is.null(n)) {
    n <- parallel_analysis(data)
    n <- n$nfact
  }
  psych::fa(cor(data), nfactors = n, fm = "ml", rotate = "geominQ")
}


#' Estimates eigenvalues for a set of variables
#'
#' Calculates eigenvalues for p number of dimensions
#'
#' @inheritParams factor_analysis
#' @examples
#'
#' ## conduct parallel analysis of polcom news use items
#' eigenvalues(polcom, news_1:news_6)
#'
#' @return A tibble of p factors, their [eigen]values, and whether the values
#'   exceed 1.0
#' @export
#' @rdname factor_analysis
eigenvalues <- function(data, ...) {
  data <- tidyselector(data, ...)
  data <- all_numeric(data)
  e <- eigen(cor(data, use = "pairwise"), only.values = TRUE, symmetric = TRUE)
  e <- tibble::data_frame(
    p = seq_along(e$values),
    value = e$values,
    `value>1.0` = e$values > 1)
  n <- sum(e[[3]])
  cat(paste0("There were ", n, " eigenvalues greater than 1.0."), fill = TRUE)
  e
}

#' Conduct a parallel analysis to determine number of factors or components
#'
#' Simulates data to use as a baseline for making factor/component retention
#' decisions
#'
#' @inheritParams factor_analysis
#'
#' @examples
#'
#' ## conduct parallel analysis of polcom news use items
#' parallel_analysis(polcom, news_1:news_6)
#'
#' @export
#' @rdname factor_analysis
parallel_analysis <- function(data, ...) {
  data <- tidyselector(data, ...)
  psych::fa.parallel(cor(data), nrow(data), fm = "ml")
}

