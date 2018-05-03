polcom %>%
  mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ pp_ideology + news_4 + ambiv_sexism_1, type = "negbinom") %>%
  tidy_summary()
