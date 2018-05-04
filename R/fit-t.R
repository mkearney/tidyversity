
t_fit <- function(x, ci = .95) {
  cilo <- paste0("lo.", gsub("0\\.", "", ci))
  cihi <- paste0("hi.", gsub("0\\.", "", 1- ci))
  diff <- x$estimate - rev(x$estimate)
  group <- gsub("mean in group ", "", names(x$estimate))
  tbl_frame(group = group,
    df = x$parameter,
    mean = x$estimate,
    diff = diff,
    !!cilo := x$conf.int[1] * sign(diff),
    !!cihi := x$conf.int[2] * sign(diff))
}


