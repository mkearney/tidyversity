
store_tidycall <- function(dims, model, type = NULL, robust = NULL) {
  ## initalize output list
  lst <- list()
  ## data dimensions
  if (is.numeric(dims)) {
    lst$data <- as.character(dims)
  } else if (is.data.frame(dims)) {
    lst$data <- dim(dims)
  } else {
    lst$data <- dim(model.frame(dims))
  }
  ## store model formula as character
  if (rlang::is_character(model)) {
    model <- strsplit(model, "\\n")[[1]]
    if (length(model) > 1L) {
      model <- paste(c(model[1],
        paste0("                ", model[-1])),
        collapse = "\n")
    }
  } else {
    model <- rlang::expr_text(formula(model))
  }
  lst$model <- model
  ## type of model
  if (is.null(type)) {
    type <- ""
  }
  lst$typetype <- std_model_type(type)
  ## whether the model is robust
  if (is.null(robust)) {
    robust <- FALSE
  }
  lst$robust <- robust
  ## return as class tidycall
  structure(lst, class = "tidycall")
}

print.tidycall <- function(x) {
  ## format model type
  if (is.null(x$type)) {
    type <- ""
  } else {
    type <- x$type
  }
  if (is_ols(type)) {
    type <- "Ordinary Least Squares (OLS) regression"
  } else if (is_logistic(type)) {
    call <- "Logistic regression"
  } else if (is_poisson(type)) {
    type <- "Poisson regression"
  } else if (is_negbinom(type)) {
    type <- "Negative binomial regression"
  }
  ## format robust (if applicable)
  if (type != "" && x$robust) {
    type <- paste0("[Robust] ", type)
  }
  ## format data dimensions print out
  if (length(x$data) == 2) {
    data <- paste0(x$data[1], " (observations) X ", x$data[2], " (variables)")
  } else {
    data <- paste0(x$data, " (observations)")
  }
  ## format data dimensions and combine with type and model
  if (type == "") {
    p <- paste0("# A tidy model\nModel formula : ", x$model,
      "\nModel data    : ", data, "\n")
  } else {
    p <- paste0("# A tidy model\nModel formula : ", x$model,
      "\nModel type    : ", type,
      "\nModel data    : ", data, "\n")
  }
  ## print model info
  cat(p, fill = TRUE)
}

get_tidycall <- function(m) {
  attr(m, "tidycall")
}
