
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

standardize_inputs <- function(x) {
  x <- model.frame(x)
  if (ncol(x) < 2L) return(as_tbl(x))
  y <- x[1]
  x <- x[-1]
  chars <- vapply(x, function(x) is.character(x) | is.factor(x), logical(1))
  x[, !chars] <- scale(x[, !chars])
  as_tbl(cbind(y, x))
}

tidy_coef <- function(x) UseMethod("tidy_coef")

tidy_coef.lm <- function(x) {
  ## estimate standardized solution
  data <- standardize_inputs(x)
  ## sometimes it won't converge, so tryCatch returns NULL
  s <- tryCatch(update(x, . ~ . - 1, data = data), error = function(e) NULL)
  ## broom the coef table, rename, and add stars column
  x <- tibble::as_tibble(broom::tidy(x), validate = FALSE)
  names(x)[2:4] <- c("est", "s.e.", "est.se")
  x <- add_stars(x)
  ## if the standardized solution worked, append the stardardized estimates
  ## otherwise return NA vector
  if (!is.null(s)) {
    s <- tibble::as_tibble(broom::tidy(s), validate = FALSE)
    if (nrow(x) > nrow(s)) {
      x$std.est <- c(0, s[[2]])
    } else {
      x$std.est <- s[[2]]
    }
  } else {
    x$std.est <- NA_real_
  }
  x
}

add_std_est <- function(d, m) {
  ## estimate standardized solution
  data <- standardize_inputs(x)
  ## sometimes it won't converge, so tryCatch returns NULL
  s <- tryCatch(update(m, . ~ . - 1, data = data), error = function(e) NULL)
  ## if the standardized solution worked, append the stardardized estimates
  ## otherwise return NA vector
  if (!is.null(s)) {
    s <- tibble::as_tibble(broom::tidy(s), validate = FALSE)
    if (nrow(d) > nrow(s)) {
      d$std.est <- c(0, s[[2]])
    } else {
      d$std.est <- s[[2]]
    }
  } else {
    d$std.est <- NA_real_
  }
  d
}

tidy_coef.default <- function(x) {
  ## broom the coef table, rename, and add stars column
  d <- tibble::as_tibble(broom::tidy(x), validate = FALSE)
  names(d)[2:4] <- c("est", "s.e.", "est.se")
  d <- add_stars(d)
  ## estimate/add standardized solution estimates and return
  add_std_est(d)
}

tidy_coef.aov <- function(x) {
  ## broom the coef table and add stars column
  d <- tibble::as_tibble(broom::tidy(x), validate = FALSE)
  add_stars(d)
}

tidy_coef.htest <- function(x) {
  coef <- broom::tidy(x)[c(1, 4, 5)]
  names(coef)[1:2] <- c("est", "t")
  add_stars(coef)
}

tidy_data <- function(x) UseMethod("tidy_data")

tidy_data.default <- function(x) {
  x <- tryCatch(broom::augment(x), error = function(e) data.frame())
  as_tbl(x)
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
  if (!is.data.frame(data)) {
    data <- tbl_frame()
  }

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

tidy_fit.htest <- function(x) t_fit(x)
