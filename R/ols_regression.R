#' Ordinary least squares (OLS) regression
#'
#' Conducts regression analysis to model outcome variable using OLS
#'
#' @param data Data frame containing variables of interest.
#' @param model Model formula to be estimated.
#' @param ... Other arguments passed to lm() function.
#' @return A list object containing a "fit" and "ceof" data frames.
#' @examples
#'
#' ## predict mpg using weight and cylinders
#' ols_regression(datasets::mtcars, mpg ~ wt + cyl)
#'
#' @export
ols_regression <- function(data, model, ...) {
  m <- lm(model, data = data)
  fit_statistics <- ols_fit(m)
  model_estimates <- broom::tidy(m)
  model_estimates <- add_stars(model_estimates)
  #preds <- get_preds(model)
  #response <- get_response(model)
  #r2s <- get_r2(data, response, preds)
  #model_estimates$r2_chg <- NA_real_
  #terms <- paste0("^", preds)
  #pt <- character(length(terms))
  #for (i in seq_len(length(terms))) {
  #  pt[i] <- gsub(terms[i], "", model_estimates$term[-1][i])
  #  pt[i] <- paste0(pt[i], "$")
  #  pt[i] <- gsub(pt[i], "", model_estimates$term[-1][i])
  #}
  #model_estimates$r2_chg[-1] <- r2s[match(pt, preds)]
  list(fit = fit_statistics,
    coef = model_estimates)
}

ols_fit <- function(m) {
  s <- summary(m)
  s$fstatistic
  anova(m)
  a <- unname(s$fstatistic)
  anm <- paste0("F(", a[2], ", ", a[3], ")")
  fp <- pf(s$fstatistic[1L],
    s$fstatistic[2L],
    s$fstatistic[3L],
    lower.tail = FALSE)
  df <- data.frame(
    statistic = c(anm, "R^2", "Adj R^2", "RMSE"),
    estimate = c(a[1], s$r.squared, s$adj.r.squared, s$sigma),
    p.value = c(fp, NA_real_, NA_real_, NA_real_),
    stringsAsFactors = FALSE,
    check.rows = FALSE,
    row.names = NULL,
    check.names = FALSE
  )
  add_stars(df)
}



build_model <- function(data, response, preds) {
  f <- paste0(response, " ~ ", paste(preds, collapse = " + "))
  lm(formula(f), data = data)
}
rsquared <- function(m) ols_fit(m)$estimate[[2]]
get_r2 <- function(data, response, preds) {
  m0 <- build_model(data, response, preds)
  r2_all <- rsquared(m0)
  r2 <- double(length(preds))
  for (i in seq_along(preds)) {
    r2[i] <- rsquared(build_model(data, response, preds[-i]))
    r2[i] <- r2_all - r2[i]
  }
  r2
}
get_preds <- function(f) strsplit(gsub("\\s+", "", as.character(f)[3]), "\\+")[[1]]
get_response <- function(f) gsub("\\s+", "", as.character(f)[2])
