polcom %>%
  dplyr::mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ pp_ideology + news_4, type = "negbinom", robust = TRUE) %>%
  tidy_summary()
f <- formula(pp_ideology ~ age + sex + hhinc)
make_tidycall(f, "pois", TRUE, polcom) %>% cat()

