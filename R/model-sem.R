
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
<<<<<<< HEAD
  ## build model formula
  model <- formulas2lavmodel(...)
  ## estimate model
  m <- lavaan::sem(model, data = .data, estimator = estimator)
  ## store info as tidycall attribute
  dims <- c(m@Data@nobs[[1]], length(m@Data@ov.names[[1]]))
  attr(m, "tidycall") <- store_tidycall(dims, model, robust = robust)
  ## return model object
  m
=======
  mod <- formulas2lavmodel(...)
  lavaan::sem(mod, data = .data, estimator = estimator)
>>>>>>> 07e023cc0fe0f69eede3439af14d6e7da0c9ca31
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
