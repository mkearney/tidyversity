polcom %>%
  dplyr::mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ pp_ideology + news_4, type = "ols", robust = FALSE) %>%
  tidy_summary() %>%
  .[1:2] %>%
  lapply(as.data.frame)

polcom %>%
  dplyr::mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ pp_ideology + news_4, type = "quasipoisson", robust = TRUE) %>%
  tidy_summary()

x <- m
tidyversity:::as_tbl(cbind(model.frame(m)[1], scale(model.frame(m)[-1])))

polcom %>%
  tidy_ttest(pp_ideology ~ follow_trump) %>%
  tidy_summary()

polcom %>%
  dplyr::mutate(vote_trump = (vote_2016_choice==1),
    sex = ifelse(sex == 1, "Male", "Female")) %>%
  tidy_regression(vote_trump ~ edu + ambiv_sexism_3 +
      img1_hrc_1 + img1_djt_1 + hhinc + age + edu + sex, "logistic") %>%
  tidy_summary()
