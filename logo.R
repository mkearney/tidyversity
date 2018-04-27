## load ggplot2
library(ggplot2)

## drawing
cap1 <- data.frame(
  x = c(-.45, 0, .45, 0),
  y = c( .50, .65, .50, .35) - .225
)
cap2 <- data.frame(
  x = c(-.28, -.28, .28, .28),
  y = c(.525, .30, .30, .525) - .225
)
strng <- data.frame(
  x = c(.00, .36, .36, .345, .33, .33, 0),
  y = c(.51, .475, .27,  .28, .26, .46, .49) - .225
)
btn <- data.frame(
  x = c(-.045, 0, .045, 0),
  y = c(.50, .53, .50, .47) - .225
)

## background/border
h1 <- hexagon::hexdf(1)
h2 <- hexagon::hexdf(.975)
h1$x <- round(h1$x, 2)
h2$x <- round(h2$x, 2)

## create and save hex sticker
ggplot(h1, aes(x, y)) +
  geom_polygon(fill = "#000000", size = 1, colour = "#000000") +
  geom_polygon(data = h2, fill = "#006677", size = 0) +
  geom_polygon(data = cap2, fill = "#222222", size = .5, colour = "#000000") +
  geom_polygon(data = cap1, fill = "#444444",  size = .5, colour = "#000000") +
  geom_polygon(data = strng, fill = "goldenrod", size = 0) +
  geom_polygon(data = btn, fill = "#000000", size = 0) +
  annotate("text", 0, -.175, label = "tidyacademic", size = 6,
    colour = "#ffffff", family = "Consolas") +
  hexagon::theme_hexagon() +
  theme(plot.margin = margin(-6, -6, -9, -8, unit = "pt")) +
  ggsave("man/figures/logo.png", width = 1.74, height = 2, units = "in",
    bg = "transparent")

usethis::use_build_ignore("logo.R")

