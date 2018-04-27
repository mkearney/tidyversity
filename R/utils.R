
tidyselector <- function(data, ...) {
  vars <- tidyselect::vars_select(names(data), ...)
  if (length(vars) > 0) {
    data <- data[vars]
  }
  data
}
