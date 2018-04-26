
library(tidyverse)

socdom <- readRDS("~/Dropbox/socdom.csv")
d <- select(socdom, follow_trump:img2_hrc_15, img1_djt_1:img2_djt_15, pinterest_1:pie_4, vote_2016:hhinc, lat = LocationLatitude, lng = LocationLongitude, state, county)

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

table(d$sex, d$vote_2016_choice)
table(d$gender)
table(d$follow_trump, d$gender)
table(d$follow_trump, d$vote_2016_choice)
table(d$pp_party, d$follow_trump)

attr(socdom, "descriptions")



eigen_values(select(d, ambiv_sexism_1:ambiv_sexism_22))

psych::fa(select(d, ambiv_sexism_1:ambiv_sexism_22), nfactors = 3)



cronbachs_alpha(ambses)

