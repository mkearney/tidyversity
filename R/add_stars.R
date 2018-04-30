
add_stars <- function(x) {
  x$stars <- dplyr::case_when(
    x$p.value < .10 & x$p.value >  .05 ~ "+  ",
    x$p.value < .05 & x$p.value >  .01 ~ "*  ",
    x$p.value < .01 & x$p.value > .001 ~ "** ",
    x$p.value < .001                   ~ "***",
    TRUE                               ~ ""
  )
  ## round p.value
  x$p.value <- round(x$p.value, 5)
  x
}
