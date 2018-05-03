
#' @export
tidy_summary <- function(m) {
  sweep(m)
}

tidy_coef <- function(x) {
  x <- tibble::as_tibble(broom::tidy(x), validate = FALSE)
  add_stars(x)
}

tidy_data <- function(x) {
  tibble::as_tibble(broom::augment(x), validate = FALSE)
}

sweep <- function(x) {
  tidy_model(fit = tidy_fit(x), coef = tidy_coef(x), data = tidy_data(x))
}

tidy_model <- setClass("tidy_model",
  slots = c(fit = "data.frame", coef = "data.frame", data = "data.frame"))

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
    print(list(fit = object@fit, coef = object@coef))
  }
)

tidy_fit <- function(x) UseMethod("tidy_fit")

tidy_fit.lm <- function(x) ols_fit(x)

tidy_fit.aov <- function(x) ols_fit(x)

tidy_fit.glm <- function(x) glm_fit(x)


