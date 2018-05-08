

## determine type from expr
type_ttest <- function(expr) {
  grepl("^t\\.test$", rlang::expr_text(expr[[1]]))
}
type_ols <- function(expr) {
  grepl("^lm$|^stats::lm$|^MASS::rlm$|^rlm$", rlang::expr_text(expr[[1]]))
}
type_anova <- function(expr) {
  grepl("^aov$|^stats::aov$", rlang::expr_text(expr[[1]]))
}
type_log <- function(expr) {
  "family" %in% names(expr) && grepl("^binomial", rlang::expr_text(expr$family))
}
type_qlog <- function(expr) {
  "family" %in% names(expr) && grepl("^quasibinomial", rlang::expr_text(expr$family))
}
type_pois <- function(expr) {
  "family" %in% names(expr) && grepl("^poisson", rlang::expr_text(expr$family))
}
type_qpois <- function(expr) {
  "family" %in% names(expr) && grepl("^quasipoisson", rlang::expr_text(expr$family))
}
type_negbin <- function(expr) {
  grepl("glm\\.nb", rlang::expr_text(expr[[1]]))
}
type_sem <- function(expr) {
  grepl("^sem$|^lavaan::sem$", rlang::expr_text(expr[[1]]))
}
type_mlm <- function(expr) {
  grepl("^rlmer$|^robustlmm::rlmer$|^lmer$|^lme4::lmer$", rlang::expr_text(expr[[1]]))
}

store_tidycall <- function(dims, expr) {
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
  ## store model from expr
  model <- expr[[2]]
  ## store model formula as character
  if (rlang::is_character(model)) {
    model <- strsplit(model, "\\n")[[1]]
    if (length(model) > 1L) {
      model <- paste(c(model[1],
        paste0("                 ", model[-1])),
        collapse = "\n")
    }
  } else {
    model <- rlang::expr_text(model)
  }
  lst$model <- model
  ## type of model
  ## extract type from expr
  if (type_ttest(expr)) {
    lst$type <- "ttest"
  } else if (type_ols(expr)) {
    lst$type <- "ols"
  } else if (type_anova(expr)) {
    lst$type <- "anova"
  } else if (type_log(expr)) {
    lst$type <- "log"
  } else if (type_qlog(expr)) {
    lst$type <- "qlog"
  } else if (type_pois(expr)) {
    lst$type <- "pois"
  } else if (type_qpois(expr)) {
    lst$type <- "qpois"
  } else if (type_negbin(expr)) {
    lst$type <- "negbin"
  } else if (type_sem(expr)) {
    lst$type <- "sem"
  } else if (type_mlm(expr)) {
    lst$type <- "mlm"
  } else {
    stop("can't determine function type")
  }
  ## whether the model is robust
  if (is_robust(expr)) {
    lst$robust <- TRUE
  } else {
    lst$robust <- FALSE
  }
  ## convert expr to pkg::fun
  lst$pkgfun <- pkgfun(expr)
  ## return as class tidycall
  structure(lst, class = "tidycall")
}

meta_call <- function() {
  fun    <- c(   "lm",  "rlm",   "aov",   "glm", "glmRob",   "glm", "glmRob",    "glm", "glmRob",    "sem",    "sem", "lmer",      "rlmm")
  type   <- c(  "ols",  "ols",      NA,   "log",    "log",  "pois",   "pois", "negbin", "negbin",       NA,       NA,     NA,          NA)
  robust <- c(  FALSE,   TRUE,   FALSE,   FALSE,     TRUE,   FALSE,     TRUE,    FALSE,     TRUE,    FALSE,     TRUE,  FALSE,        TRUE)
  pkg    <- c("stats", "MASS", "stats", "stats", "robust", "stats", "robust",  "stats", "robust", "lavaan", "lavaan", "lme4", "robustlmm")
  tbl_frame(pkg, fun, robust, type)
}



pkg_models <- function(fun = NULL, type = NULL, robust = NULL, pkg = NULL) {
  mc <- meta_call()
}

print.tidycall <- function(x) {
  ## format model type
  if (is.null(x$type)) {
    type <- ""
  } else {
    type <- x$type
  }
  if (is_ttest(type)) {
    type <- "T-test"
  } else if (is_ols(type)) {
    type <- "Ordinary Least Squares (OLS) regression"
  } else if (is_anova(type)) {
    type <- "Analysis of variance (ANOVA)"
  } else if (is_log(type)) {
    type <- "Logistic regression"
  } else if (is_qlog(type)) {
    type <- "Quasi-logistic regression"
  } else if (is_pois(type)) {
    type <- "Poisson regression"
  } else if (is_qpois(type)) {
    type <- "Quasi-poisson regression"
  } else if (is_negbin(type)) {
    type <- "Negative binomial regression"
  } else if (is_sem(type)) {
    type <- "Structural Equation Model (SEM)"
  } else if (is_mlm(type)) {
    type <- "Multilevel Model (MLM)"
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
  p <- paste0("# A tidy model",
    "\nModel formula  : ", x$model,
    "\nModel type     : ", type,
    "\nModel pkg::fun : ", paste0(x$pkgfun, "()"),
    "\nModel data     : ", data, "\n")
  cat(p, fill = TRUE)
}

get_tidycall <- function(m) {
  attr(m, "tidycall")
}




## get package of first function for a given expression
pkgfun <- function(expr) {
  ## validate input
  stopifnot(rlang::is_expression(expr))
  ## convert function to text
  expr <- rlang::expr_text(expr[[1]])
  ## if namespace already attached, return it otherwise look it up
  if (!grepl("\\:\\:", expr)) {
    ## lookup and return name of namespace
    #pkg <- rlang::env_name(rlang::fn_env(rlang::as_function(expr)))
    pkg <- rlang::ns_env_name(rlang::fn_env(rlang::as_function(expr)))
    ## remove "namespace:", only use pkg name
    pkg <- gsub(".*:", "", pkg)
    ## combine with namespace
    expr <- paste0(pkg, "::", expr)
  }
  expr
}
