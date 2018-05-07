
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
  ## build model formula
  model <- formulas2lavmodel(...)
  ## capture model expression
  e <- rlang::expr(lavaan::sem(model, data = .data, estimator = estimator))
  ## estimate model
  m <- eval(e)
  ## store info as tidycall attribute
  dims <- c(m@Data@nobs[[1]], length(m@Data@ov.names[[1]]))
  attr(m, "tidycall") <- store_tidycall(dims, e)
  ## return model object
  m
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
