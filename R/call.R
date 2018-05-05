
store_tidycall <- function(m, model, type = NULL, robust = NULL) {
  if (is.numeric(m)) {
    data <- as.character(m)
  } else {
    data <- dim(model.frame(m))
  }
  lst <- list(data = data,
    model = rlang::expr_text(formula(model)))
  if (is.null(type)) {
    type <- ""
  }
  if (is.null(robust)) {
    robust <- FALSE
  }
  lst$type <- std_model_type(type)
  lst$robust <- robust
  structure(lst, class = "tidycall")
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
  }
  if (type != "" && x$robust) {
    type <- paste0("[Robust] ", type)
  }

  if (length(x$data) == 2) {
    data <- paste0(x$data[1], " (observations) X ", x$data[2], " (variables)")
  } else {
    data <- paste0(x$data, " (observations)")
  }
  if (type == "") {
    p <- paste0("# A tidy model\nModel formula : ", x$model,
      "\nModel data    : ", data, "\n")
  } else {
    p <- paste0("# A tidy model\nModel formula : ", x$model,
      "\nModel type    : ", type,
      "\nModel data    : ", data, "\n")
  }
  cat(p, fill = TRUE)
}

get_tidycall <- function(m) {
  attr(m, "tidycall")
}
