## function to genrate latent indicator data frame
latent_indicators <- function(l, i, n, m = 0, sd = 1) {
  r_ <- function(lv = NULL) {
    if (is.null(lv)) {
      lv <- 0
    }
    lv + rnorm(n, m, sd)
  }
  r <- function(ni, nm) {
    nm <- paste0(nm, seq_len(ni))
    lv <- rnorm(n)
    d <- as_tbl(replicate(ni, r_(lv)))
    names(d) <- nm
    d
  }
  as_tbl(do.call("cbind", Map(r, i, l)))
}

## create data set
d <- latent_indicators(c("x", "y", "z"), c(3, 4, 5), 200)

data %>%
  tidy_sem(x =~ x1 + x2 + x3,
    y =~ y1 + y2 + y3 + y4,
    z =~ z1 + z2 + z3 + a*z4 + b*z5,
    y ~ x,
    z ~ x + y + z) %>%
  tidy_summary()
