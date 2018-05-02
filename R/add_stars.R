
add_stars <- function(x) {
  x$stars <- make_stars(x)
  ## round p.value
  ##x$p.value <- round(x$p.value, 5)
  x
}


make_stars <- function(x) UseMethod("make_stars")

make_stars.data.frame <- function(x) {
  if ("p.value" %in% names(x)) {
    x <- x$p.value
  } else if (any(grepl("^p$|^pval$|^pvalue$", names(x)))) {
    x <- x[[grep("^p$|^pval$|^pvalue$", names(x))[1]]]
  }
  make_stars(x)
}

make_stars.numeric <- function(x) {
  dplyr::case_when(
    x < .10 & x >  .05 ~ "+",
    x < .05 & x >  .01 ~ "*",
    x < .01 & x > .001 ~ "**",
    x < .001           ~ "***",
    TRUE               ~ ""
  )
}

make_stars.character <- function(x) {
  x <- as.numeric(x)
  make_stars(x)
}
