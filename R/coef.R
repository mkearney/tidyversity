coef_default <- function(x) {
  ## broom the coef table, rename, and add stars column
  d <- tibble::as_tibble(broom::tidy(x), validate = FALSE)
  names(d)[2:4] <- c("est", "s.e.", "est.se")
  d <- add_stars(d)
  ## estimate/add standardized solution estimates and return
  add_std_est(d)
}

coef_lm <- function(x) {
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

coef_aov <- function(x) {
  ## broom the coef table and add stars column
  d <- as_tbl(broom::tidy(x))
  add_stars(d)
}

coef_htest <- function(x) {
  coef <- broom::tidy(x)[c(1, 4, 5)]
  names(coef)[1:2] <- c("est", "t")
  add_stars(coef)
}

coef_lavaan <- function(m) {
  m <- as_tbl(lavaan::parameterEstimates(m))
  m$term <- apply(m[, 1:3], 1, paste, collapse = " ")
  m <- m[, c(11, 5:10)]
  names(m)[4:5] <- c("est.se", "p.value")
  add_stars(m)
}


