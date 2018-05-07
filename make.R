
table(polcom$pp_party)

polcom %>%
  tidy_regression(ambiv_sexism_1>3 ~ pp_party + sex, type = "logistic", robust = TRUE) -> m
  tidy_summary()
