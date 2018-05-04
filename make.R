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

