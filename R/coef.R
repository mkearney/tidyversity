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

coef_lavaan2 <- function(m) {
  m <- as_tbl(lavaan::parameterEstimates(m, standardized = TRUE))
  ## create term var
  m$term <- apply(m[, 1:3], 1, paste, collapse = " ")
  chars <- nchar(gsub("\\s.*", "", m$term))
  mchars <- max(chars)
  spaces <- function(n) paste(rep(" ", n), collapse = "")
  sp <- sapply(mchars - chars + 1, spaces)
  m$term <- unlist(Map(sub, "\\s", sp, m$term))
  ## store lhs and op for subsetting
  lhs <- m$lhs
  op <- m$op
  ## select and rename variables
  m <- m[, c("term", "est", "se", "z", "pvalue",  "std.all")]
  names(m)[4:6] <- c("est.se", "p.value", "std.est")
  ## add stars and rearrange
  m <- add_stars(m)
  m <- m[, c("term", "est", "se", "est.se", "p.value", "stars", "std.est")]
  ## residuals
  r <- m[op == "~~", ]
  cat("## Residuals: ", fill = TRUE)
  print_no_tibble(r)
  ## latent loadings
  l <- m[op == "=~", ]
  cat("\n## Latent variable: ", fill = TRUE)
  print_no_tibble(l)
  ## structural coef
  s <- m[op == "~", ]
  sl <- lhs[op == "~"]
  sn <- unique(sl)
  for (i in sn) {
    cat("\n## Regression: ", fill = TRUE)
    print_no_tibble(s[sl == i, ])
  }
}

print_no_tibble <- function(x, n = NULL,
                            dims = FALSE,
                            class = FALSE,
                            row.names = FALSE,
                            leading_zero = TRUE) {
  x <- no_tibble(x, n, dims, class, row.names, leading_zero)
  cat(paste(x, collapse = "\n"))
}

replace_missing <- function(x, sub = -99999.99) {
  x[is.na(x)] <- sub
  x
}

format_ncol <- function(x) {
  x <- sprintf("%.3f ", x)
  #x <- sub("^(?!-)", " ", x, perl = TRUE)
  x[grepl("^\\s{0,}NA\\s{0,}$", x)] <- ""
  chars <- nchar(trimws(x))
  mchars <- max(chars)
  spaces <- function(n) paste(rep(" ", n), collapse = "")
  sp <- sapply(mchars - chars, spaces)
  x <- unlist(Map(paste0, sp, x, USE.NAMES = FALSE))
  m <- floor(mchars / 2)
  sub(sprintf("(?<=\\s{%d}).(?=\\s)", m, m), "-", x, perl = TRUE)
}

no_tibble <- function(x, n = NULL,
                      dims = FALSE,
                      class = FALSE,
                      row.names = FALSE,
                      leading_zero = TRUE) {
  ## round to 3 past decimal
  is_num <- vapply(x, is.numeric, logical(1))
  #x[is_num] <- lapply(x[is_num], replace_missing)
  x[is_num] <- lapply(x[is_num], format_ncol)
  ## count obs
  nobs <- nrow(x)
  ## set n to obs if null
  if (is.null(n)) {
    n <- nobs
  }
  ## capture as text string
  x <- capture.output(print(x, n = n))
  ## show column class
  if (!class) {
    x <- x[-3]
  }
  ## strip dimensions (A tibble...) line
  if (!dims) {
    x <- x[-1]
  }
  ## remove row names
  if (!row.names) {
    n <- nchar(nobs)
    x <- gsub(sprintf("^\\s{0,%d}\\d+", n), paste(rep(" ", n), collapse = ""), x)
    x <- sub("\\s+", "", x)
  }
  ## replace quotes with blanks
  x <- gsub('"', " ", x)
  ## replace ticks with blanks
  x <- gsub("`", " ", x)
  ## replace 0.000 with <.001
  x <- gsub("0\\.000", "<.001", x)
  ## remove leading zeroes
  if (!leading_zero) {
    x <- gsub(" 0\\.", "  .", x)
    x <- gsub(" -0\\.", "   .", x)
  }
  ## trim white space from right end
  x <- gsub("\\s+$", "", x)
  ## realign names
  while (nchar(x[2]) < nchar(x[1])) {
    x[1] <- sub("\\s", "", x[1])
  }
  ## fix heading spacing
  x[1] <- sub("\\best\\s{4}", "    est", x[1])
  x[1] <- sub("\\bse\\s{3}", "    se", x[1])
  x[1] <- sub("\\s{1}p\\.value", "p.value ", x[1])
  x[1] <- sub("\\s{1}stars", "stars ", x[1])
  ## print
  hline <- paste(rep("-", max(nchar(x))), collapse = "")
  c(x[1], hline, x[-1])
}
