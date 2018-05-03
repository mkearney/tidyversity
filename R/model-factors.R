
#' Conduct factor analysis to estimate item loadings on each factor
#'
#' Provides estimates from a rotated factor analysis.
#'
#' @param data Data frame of interest
#' @param ... Variables (unquoted) of interest. If not provided, all variables
#'   in data will be used
#' @param nfactors Method used to estimate the number of factors. The default,
#'   uses a "parallel" analysis to estimate the number of retained factors.
#'   Also available is the "kaiser" method, which retains all eigenvalues
#'   greater than 1.0. Users may also manually specify the number of factors
#'   by entering a numeric [or integer] value here.
#' @param rotation Method used to estimate rotated solution. The default
#'   "oblique" rotation method (geominQ from GPArotation pkg) assumes the
#'   factors are related (a fairly typical or desirable assumption of social
#'   science constructs). Also available is the "orthogonal" rotation method
#'   (geominT from the GPArotation pkg), which assumes the factors are
#'   independent. Users may override these options, providing their own
#'   rotation method by specifying [presumably a method offered in the psych
#'   or GPArotation packages] here as well.
#' @return Output of factor analysis.
#' @examples
#'
#' ## conduct parallel analysis of polcom news use items
#' factor_analysis(polcom, news_1:news_6)
#'
#' @export
#' @rdname factor_analysis
factor_analysis <- function(data, ...,
                            nfactors = "parallel",
                            rotation = "oblique",
                            method = NULL) {
  UseMethod("factor_analysis")
}

unlist_data_frame <- function(x) {
  unlist(x, use.names = FALSE)
}

#' @export
factor_analysis.data.frame <- function(data, ...,
                                       nfactors = "parallel",
                                       rotation = "oblique",
                                       method = NULL) {
  data <- tidyselector(data, ...)
  ## detect nfactors method and set number of factors
  if (nfactors == "parallel") {
    n <- parallel_analysis(data)
    n <- n$nfact
  } else if (nfactors == "kaiser") {
    n <- eigenvalues(data)
    n <- sum(n[[3]])
  } else {
    n <- nfactors
  }
  if (rotation == "oblique") {
    rotation <- "geominQ"
  } else if (rotation == "orthogonal") {
    rotation <- "geominT"
  }
  if (is.null(method)) {
    ## check normality of data
    s <- shapiro.test(unlist_data_frame(data))
    if (s$p.value > .05) {
      method <- "ml"
    } else {
      method <- "gls"
    }
  }
  ## validate n
  stopifnot(is.numeric(n))
  ## factor analysis
  psych::fa(cor(data), nfactors = n, fm = method, rotate = rotation)
}


## validate class of data (make sure it's a data frame or list of data frames)
#' @export
factor_analysis.default <- function(data, ...,
                                    nfactors = "parallel",
                                    rotation = "oblique",
                                    method = NULL) {
  stop("data is not a data frame!", call. = FALSE)
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

