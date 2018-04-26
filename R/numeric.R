all_numeric <- function(x) {
  x[1:ncol(x)] <- lapply(x, coerce_numeric)
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
