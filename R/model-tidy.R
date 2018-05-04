
store_tidycall <- function(m, model, type, robust) {
  structure(
    list(data = dim(model.frame(m)),
      model = rlang::expr_text(formula(model)),
      type = std_model_type(type),
      robust = robust),
    class = "tidycall")
}

print.tidycall <- function(x) {
  type <- x$type
  if (is_ols(type)) {
    type <- "Ordinary Least Squares (OLS) regression"
  } else if (is_logistic(type)) {
    call <- "Logistic regression"
  } else if (is_poisson(type)) {
    type <- "Poisson regression"
  } else if (is_negbinom(type)) {
    type <- "Negative binomial regression"
  } else {
    stop("cannot recognized type", call. = FALSE)
  }
  if (x$robust) {
    type <- paste0("[Robust] ", type)
  }
  data <- paste0(x$data[1], " (observations) X ", x$data[2], " (variables)")
  cat(paste0("# A tidy model\nModel formula : ", x$model,
       "\nModel type    : ", type,
       "\nModel data    : ", data, "\n"), fill = TRUE)
}

get_tidycall <- function(m) {
  attr(m, "tidycall")
}

#' @export
tidy_summary <- function(m) {
  print(get_tidycall(m))
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


