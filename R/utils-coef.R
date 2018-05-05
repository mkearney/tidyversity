standardize_inputs <- function(x) {
  x <- model.frame(x)
  if (ncol(x) < 2L) return(as_tbl(x))
  y <- x[1]
  x <- x[-1]
  chars <- vapply(x, function(x) is.character(x) | is.factor(x), logical(1))
  x[, !chars] <- scale(x[, !chars])
  as_tbl(cbind(y, x))
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
