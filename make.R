lmfit <- lm(mpg ~ wt, datasets::mtcars)
library(broom)
tidy(lmfit)

head(augment(lmfit))



library(tidyverse)

## predict mpg using weight and cylinders
m <- lm(mpg ~ wt + cyl, data = datasets::mtcars)
s <- summary(m)
s$fstatistic
ols_regression(datasets::mtcars, mpg ~ wt + cyl)
tibble:::print.trunc_mat

tu <- ols_regression(data = datasets::mtcars, mpg ~ wt + cyl)
class(tu)

?broom::glance
?summary.lm
usethis::use_pipe()

## working on polcom_survey data set
socdom <- readRDS("~/Dropbox/socdom.csv")
d <- select(socdom, follow_trump:img2_hrc_15, img1_djt_1:img2_djt_15,
  pinterest_1:pie_4, vote_2016:hhinc, lat = LocationLatitude,
  lng = LocationLongitude, state, county)

ambses <- select(d, ambiv_sexism_2, ambiv_sexism_5, ambiv_sexism_10,
  ambiv_sexism_11, ambiv_sexism_14, ambiv_sexism_16)

d$follow_trump <- case_when(
  d$follow_trump == 2 ~ TRUE,
  TRUE ~ FALSE
)

d <- select(d, follow_trump:news_6,
  ambiv_sexism_1 = ambiv_sexism_2,
  ambiv_sexism_2 = ambiv_sexism_5,
  ambiv_sexism_3 = ambiv_sexism_10,
  ambiv_sexism_4 = ambiv_sexism_11,
  ambiv_sexism_5 = ambiv_sexism_14,
  ambiv_sexism_6 = ambiv_sexism_16,
  img1_hrc_1:img2_djt_15, pie_1:hhinc)

polcom <- d
save(polcom, file = "data/polcom.rda")

## scree plot function?
p <- ggplot(e, aes(x = p, y = eigenvalue)) +
  geom_point(size = 2) + geom_line() +
  geom_hline(yintercept = 1.0, colour = "red", linetype = "dashed") +
  theme(legend.position = "none") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, nrow(e))) +
  scale_y_continuous(breaks = seq(0, max(e$eigenvalue)))

glm_fit(m)


usethis::use_build_ignore("make.R")

summary(m)
s$df.null

factor_analysis(polcom, news_1:news_6)
s$df
?summary.glm
news <- cronbachs_alpha(polcom, news_1:news_6)

polcom %>%
  tidy_regression(pp_ideology ~ news_1 + ambiv_sexism_1 + pie_1 + hhinc + age, type = "ols") %>%
  tidy_summary()

f <- formula(pp_ideology ~ news_1:news_6)
rlang::f_rhs(f)

read_model(, polcom)

logistic_regression(polcom, pp_ideology>4 ~ news_1 + hhinc + sex)
m
m <- glm(pp_ideology>4 ~ news_1 + hhinc + sex, data = polcom, family = binomial)
s <- summary(m)
logLik(m)
s$deviance
nagelkerke(m)
nagelkerke(m)
