
#' Structural equation model
#'
#' Conduct latent variable model analysis (structural equation modeling)
#'
#' @param data Data frame containing variables in model
#' @param ... Model formula to be estimated.
#' @param robust Logial indicating whether to use a robust estimator.
#' @return A model object
#' @examples
#' ## specify sem model and estimate
#' m1 <- polcom %>%
#'   tidy_sem_model(news =~ news_1 + news_2 + news_3 + news_4 + news_5 + news_6,
#'     ambiv_sexism =~ ambiv_sexism_1 + ambiv_sexism_2 + ambiv_sexism_3 +
#'     ambiv_sexism_4 + ambiv_sexism_5 + ambiv_sexism_6,
#'     partisan =~ a*therm_1 + a*therm_2,
#'     ambiv_sexism ~ age + sex + hhinc + edu + news + partisan)
#'
#' ## view summary
#' m1 %>%
#'   tidy_summary()
#'
#' @export
tidy_sem <- function(.data, model = NULL, robust = FALSE, ...) {
  if (is.null(model) && !is.null(get_model(.data))) {
    model <- get_model(.data)
  }
  if (is.null(model)) {
    stop(paste0("model not found. Try using `tidy_sem_model(...)` ",
      "before `tidy_sem()`, e.g.,\ndata %>%\n  ",
      "tidy_sem_model(x =~ x1 + x2 + x3,\n    y =~ y1 + y2 + y3,\n    ",
      "y ~ x) %>%\n  tidy_sem()"))
  }
  if (robust) {
    estimator <- "mlr"
  } else {
    estimator <- "ml"
  }
  ## capture model expression
  e <- rlang::expr(lavaan::sem(!!model, data = .data, estimator = !!estimator))
  ## estimate model
  m <- eval(e)
  ## store info as tidycall attribute
  dims <- c(m@Data@nobs[[1]], length(m@Data@ov.names[[1]]))
  attr(m, "tidycall") <- store_tidycall(dims, e)
  ## return model object
  m
}

#' Build SEM model
#'
#' Specify a latent variable model, to be used prior to \code{\link{tidy_sem}}
#'
#' @param .data Data frame with variables of interest
#' @param ... Model formulas following a slightly modified version of lavaan
#'   model syntax (see \code{\link[lavaan]{lavParseModelString}}). Instead of
#'   taking only a single character string representing the specified model
#'   (as would be done with lavaan), this function accepts unquoted formulas
#'   that use commas between variable/equation lines instead of line breaks
#'   (note: line breaks may still be used for aesthetic/spacing purposes, but
#'   they otherwise have no affect on how the model is processed).
#' @return Returns the provided data with a "model" attribute
#' @export
tidy_sem_model <- function(.data, ...) {
  ## capture model input
  x <- rlang::enquos(...)
  ## if not already a single character string convert to one
  if (!(length(x) == 1L && rlang::is_character(rlang::eval_tidy(x[[1]])))) {
    x <- vapply(x, rlang::f_text, FUN.VALUE = character(1))
    lv <- vapply(x, function(i) grepl("^~", i), FUN.VALUE = logical(1))
    x[lv] <- paste0(names(x[lv]), " =~ ", gsub("^~", "", x[lv]))
    x <- paste(x, collapse = "\n")
  } else {
    x <- rlang::eval_tidy(x[[1]])
  }
  ## store model as attribute
  attr(.data, "model") <- x
  ## return data
  .data
}

get_model <- function(x) attr(x, "model")

tidy_summary_lavaan <- function(m) {
  lavaan::summary(m, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
}
