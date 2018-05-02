sweep <- function(x) {
  coef <- broom::tidy(x)
  fit <- tidy_fit(x)
  data <- broom::augment(x)
  x <- list(fit = fit, coef = coef, data = data)
  x <- lapply(x, tibble::as_tibble, validate = FALSE)
  tidy_model(x)
}

tidy_model_ <- setClass("tidy_model",
  slots = c(fit = "data.frame", coef = "data.frame", data = "data.frame"))

tidy_model <- function(...) {
  args <- list(...)
  if (length(args) == 1 && is.null(names(args))) {
    args <- args[[1]]
  }
  do.call("tidy_model_", args)
}

#' @export
print.tidy_model <- function(x) {
  cat("$data", fill = TRUE)
  print(x@data, n = 2)
  cat("\n")
  x <- list(fit = x@fit, coef = x@coef)
  print(x)
}

model_data <- function(x) UseMethod("model_data")

model_data.tidy_model <- function(x) x@data

#' @export
setMethod("show", "tidy_model",
  function(object) {
    cat("$data", fill = TRUE)
    print(object@data, n = 2)
    cat("\n")
    x <- list(fit = object@fit, coef = object@coef)
    print(x)
  }
)

tidy_fit <- function(x) UseMethod("tidy_fit")

tidy_fit.lm <- function(x) ols_fit(x)

tidy_fit.glm <- function(x) glm_fit(x)


