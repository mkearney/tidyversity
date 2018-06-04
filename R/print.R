#' @export
print.tidy_model <- function(x, ...) {
  ## space, label, and print fit
  cat("$fit", fill = TRUE)
  print_no_tibble(x$fit)
  ## space, label, and print coef
  cat("\n$coef", fill = TRUE)
  print_no_tibble(x$coef)
}


print_no_tibble <- function(x, n = NULL,
  dims = FALSE,
  class = FALSE,
  row.names = FALSE,
  leading_zero = TRUE) {
  x <- no_tibble(x, n, dims, class, row.names, leading_zero)
  cat(paste(x, collapse = "\n"), fill = TRUE)
}

trim_ws <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

format_ncol <- function(x, nm) {
  ## 3 places past decimal
  x <- sprintf("%.3f ", x)
  ## replace NA with blanks
  x[grepl("^\\s{0,}NA\\s{0,}$", x)] <- "-----" ##"-    "
  ## trim white space and calc characters
  chars <- nchar(trim_ws(x))
  ## max char to determine spacing
  mchars <- max(c(chars, nchar(nm)), na.rm = TRUE)
  ## function to create string of spaces
  spaces <- function(n) paste(rep(" ", n), collapse = "")
  ## use diff bw chars and max for number of spaces
  sp <- sapply(mchars - chars, spaces)
  ## map paste0 to add spaces before value
  x <- unlist(Map(paste0, sp, x, USE.NAMES = FALSE))
  ## replace placeholder and return
  gsub("-----", " -   ", x)
}

format_icol <- function(x, nm) {
  m <- max(c(nchar(x), nchar(nm)), na.rm = TRUE)
  if (m < 4) {
    m <- 4
  }
  x <- sprintf(paste0("%", m, "g"), x)
  ## replace NA with blanks
  x[grepl("^\\s{0,}NA\\s{0,}$", x)] <- paste0(
    paste(rep(" ", m - 1), collapse = ""), "-")
  ## trim white space and calc characters
  x
}

format_col_nm <- function(x, nm) {
  m <- max(nchar(c(x, nm)), na.rm = TRUE)
  if (m < 4) {
    m <- 4
  }
  if (nchar(nm) < m) {
    n <- m - nchar(nm)
    nm <- paste0(paste(rep(" ", n), collapse = ""), nm)
  }
  nm
}

no_tibble <- function(x, n = NULL,
                      dims = FALSE,
                      class = FALSE,
                      row.names = FALSE,
                      leading_zero = TRUE) {
  if (all(c("est", "est.se") %in% names(x))) {
    coef <- TRUE
  } else {
    coef <- FALSE
  }
  if ("stars" %in% names(x)) {
    x$stars[x$stars == ""] <- "---"
  }
  ## format numeric columns
  is_num <- vapply(x, inherits, "numeric", FUN.VALUE = logical(1))
  x[is_num] <- Map(format_ncol, x[is_num], names(x)[is_num])
  ## format int columns
  is_int <- vapply(x, inherits, "integer", FUN.VALUE = logical(1))
  x[is_int] <- Map(format_icol, x[is_int], names(x)[is_int])
  ## fix names
  nmn <- sapply(x, function(x) max(nchar(x), na.rm = TRUE))
  names(x)[is_num] <- unlist(Map(format_col_nm, x[is_num], names(x)[is_num]))
  names(x)[is_int] <- unlist(Map(format_col_nm, x[is_int], names(x)[is_int]))
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
  if (!row.names) {
    ## remove row names
    x[-1] <- sub(sprintf("^.{%d}", nchar(nobs) + 1), "", x[-1])
    x[1] <- sub(sprintf("^\\s{0,%d}", nchar(nobs) + 1), "", x[1])
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
  x <- sub("\\s+$", "", x)
  ## fix stars place holder
  x <- gsub("---", "   ", x)
  ## realign names
  while (max(nchar(x[-1]), na.rm = TRUE) > nchar(x[1])) {
    x[1] <- sub("\\s(?=\\s)", "", x[1], perl = TRUE)
  }
  ## fix heading spacing for coef data
  if (coef) {
    x[1] <- sub("\\sest", "est", x[1])
  }
  ## return
  x
}
