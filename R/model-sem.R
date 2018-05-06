
#' Structural equation model
#'
#' Conduct latent variable model analysis (structural equation modeling)
#'
#' @param data Data frame containing variables in model
#' @param ... Model formula to be estimated.
#' @param robust Logial indicating whether to use a robust estimator.
#' @return A model object
#' @export
tidy_sem <- function(.data, ..., robust = FALSE) {
  if (robust) {
    estimator <- "mlr"
  } else {
    estimator <- "ml"
  }
  mod <- formulas2lavmodel(...)
  lavaan::sem(mod, data = .data, estimator = estimator)
}

formulas2lavmodel <- function(...) {
  x <- vapply(rlang::quos(...), rlang::f_text, FUN.VALUE = character(1))
  lv <- vapply(x, function(i) grepl("^~", i), FUN.VALUE = logical(1))
  x[lv] <- paste0(names(x[lv]), " =~ ", gsub("^~", "", x[lv]))
  paste(x, collapse = "\n")
}

tidy_summary_lavaan <- function(m) {
  lavaan::summary(m, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
}
