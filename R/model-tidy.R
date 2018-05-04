
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
  tidy_model(m)
}

tidy_coef <- function(x) {
  x <- tibble::as_tibble(broom::tidy(x), validate = FALSE)
  names(x)[2:4] <- c("estimate", "s.e.", "est.se")
  add_stars(x)
}

tidy_data <- function(x) {
  tibble::as_tibble(broom::augment(x), validate = FALSE)
}

tidy_model <- function(m) {
  new_tidy_model(
    fit  = tidy_fit(m),
    coef = tidy_coef(m),
    data = tidy_data(m)
  )
}

new_tidy_model <- function(fit, coef, data) {
  stopifnot(is.data.frame(fit))
  stopifnot(is.data.frame(coef))
  stopifnot(is.data.frame(data))

  structure(
    list(
      fit = fit,
      coef = coef,
      data = data
    ),
    class = "tidy_model"
  )
}

#' @export
print.tidy_model <- function(x, ...) {
  print(x[names(x) %in% c("fit", "coef")])
}

model_data <- function(x) UseMethod("model_data")

model_data.tidy_model <- function(x) x$data

tidy_fit <- function(x) UseMethod("tidy_fit")

tidy_fit.lm <- function(x) ols_fit(x)

tidy_fit.aov <- function(x) ols_fit(x)

tidy_fit.glm <- function(x) glm_fit(x)


