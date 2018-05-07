

tidyselector <- function(data, ...) {
  vars <- tidyselect::vars_select(names(data), ...)
  if (length(vars) > 0) {
    data <- data[vars]
  }
  data
}

all_numeric <- function(x) {
  x[1:ncol(x)] <- lapply(x, coerce_numeric)
  x
}

#' @export
coerce_numeric <- function(x) UseMethod("coerce_numeric")

#' @export
coerce_numeric.default <- function(x) {
  as.numeric(x)
}

#' @export
coerce_numeric.character <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) {
    stop("You've included a character (textual) variable. This function expected only numeric, integer, or factor variables.",
      call. = FALSE)
  }
  x
}


add_stars <- function(x) {
  x$stars <- make_stars(x)
  ## round p.value
  x$p.value <- round(x$p.value, 6)
  x
}


make_stars <- function(x) UseMethod("make_stars")

make_stars.data.frame <- function(x) {
  if ("p.value" %in% names(x)) {
    x <- x$p.value
  } else if (any(grepl("^p$|^pval$|^pvalue$", names(x)))) {
    x <- x[[grep("^p$|^pval$|^pvalue$", names(x))[1]]]
  }
  make_stars(x)
}

make_stars.numeric <- function(x) {
  ifelse(
    is.na(x),            "",
  ifelse(
    x < .10 & x >=  .05, "+",
  ifelse(
    x < .05 & x >=  .01, "*",
  ifelse(
    x < .01 & x >= .001, "**",
  ifelse(
    x <= .001          , "***", ""
  )))))
}

make_stars.character <- function(x) {
  x <- as.numeric(x)
  make_stars(x)
}
