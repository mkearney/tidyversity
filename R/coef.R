coef_default <- function(x) {
  ## broom the coef table, rename, and add stars column
  d <- tibble::as_tibble(broom::tidy(x), validate = FALSE)
  names(d)[2:4] <- c("est", "s.e.", "est.se")
  d <- add_stars(d)
  ## estimate/add standardized solution estimates and return
  add_std_est(d)
}

#' @importFrom MASS rlm
#' @importFrom robust glmRob
coef_lm <- function(x) {
  ## estimate standardized solution
  data <- standardize_inputs(x)
  ## store df.residual if available
  if ("df.residual" %in% names(x)) {
    dfr <- length(x$residuals) - length(x$coefficients)
  } else {
    dfr <- NULL
  }
  ## sometimes it won't converge, so tryCatch returns NULL
  if (attr(x$terms, "intercept") == 1) {
    s <- tryCatch(suppressWarnings(update(x, . ~ . - 1, data = data)), error = function(e) NULL)
    ## if null try again this time w/o removing the intercept
    if (is.null(s)) {
      s <- tryCatch(update(x, . ~ ., data = data), error = function(e) NULL)
    }
  } else {
    s <- tryCatch(update(x, . ~ ., data = data), error = function(e) NULL)
  }
  ## broom the coef table, rename, and add stars column
  x <- tibble::as_tibble(broom::tidy(x), validate = FALSE)
  names(x)[2:4] <- c("est", "s.e.", "est.se")
  if (ncol(x) == 4 && !is.null(dfr)) {
    x$p.value <- 2 * pt(abs(x[[4]]), dfr, lower.tail = FALSE)
  }
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
  coef <- as_tbl(broom::tidy(x)[c(1, 4, 5)])
  names(coef)[1:2] <- c("est", "t")
  add_stars(coef)
}

coef_lavaan <- function(m) {
  m <- as_tbl(lavaan::parameterEstimates(m, standardized = TRUE))
  m <- m[m$op != "~~", ]
  m$term <- apply(m[, 1:3], 1, paste, collapse = " ")
  m <- m[, c("term", "est", "se", "z", "pvalue",  "std.all")]
  names(m)[4:6] <- c("est.se", "p.value", "std.est")
  m <- add_stars(m)
  m[, c("term", "est", "se", "est.se", "p.value", "stars", "std.est")]
}

