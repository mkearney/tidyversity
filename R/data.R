data_lavaan <- function(x) {
  d <- as_tbl(x@Data@X[[1]])
  names(d) <- x@Data@ov.names[[1]]
  p <- as_tbl(lavaan::predict(x))
  nms <- names(p)
  names(p) <- paste0(".", nms)
  as_tbl(cbind(d, p))
}
