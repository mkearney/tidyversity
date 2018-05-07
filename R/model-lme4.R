
#' Multilevel model
#'
#' Conduct multilevel model analysis (AKA mixed models)
#'
#' @param data Data frame containing variables in model
#' @param ... Model formula to be estimated
#' @param robust Logial indicating whether to use a robust estimator
#' @return A model object
#' @export
tidy_mlm <- function(.data, model, robust = FALSE, ...) {
  ## if robust, use rlmm pkg
  if (robust) {
    dots <- list(...)
    if ("REML" %in% names(dots) && !isTRUE(REML)) {
      stop("`REML = FALSE` with robust linear mixed models", call. = FALSE)
    }
    m <- robustlmm::rlmer(model, data = .data, ...)
  } else {
    ## estimate model
    m <- lme4::lmer(model, data = .data, ...)
  }
  ## store info as tidycall attribute
  attr(m, "tidycall") <- store_tidycall(dim(m@frame), model, robust = robust)
  ## return model object
  m
}
