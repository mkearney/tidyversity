
make_tidycall <- function(model) {
  f <- as.character(rlang::quo(!!model))
  paste0("Model formula: ", f[2], "\n")
}

get_tidycall <- function(m) {
  attr(m, "tidycall")
}

#' @export
tidy_summary <- function(m) {
  cat(get_tidycall(m), fill = TRUE)
  sweep(m)
}

tidy_coef <- function(x) {
  x <- tibble::as_tibble(broom::tidy(x), validate = FALSE)
  names(x)[2:4] <- c("estimate", "s.e.", "est.se")
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

model_data <- function(x) UseMethod("model_data")

model_data.tidy_model <- function(x) x@data

setMethod("show", "tidy_model",
  function(object) {
    #cat("$data", fill = TRUE)
    #print(object@data, n = 2)
    #cat("\n")
    print(list(fit = object@fit, coef = object@coef))
  }
)

tidy_fit <- function(x) UseMethod("tidy_fit")

tidy_fit.lm <- function(x) ols_fit(x)

tidy_fit.aov <- function(x) ols_fit(x)

tidy_fit.glm <- function(x) glm_fit(x)


