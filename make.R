

library(tidyversity)
table(polcom$pp_party)

x <- tidyversity:::fit_glmRob(m)
x
sapply(x, is.double)

polcom %>%
  tidy_regression(ambiv_sexism_1>3 ~ pp_party + sex, 
    type = "logistic", robust = TRUE) -> m
  tidy_summary()



sem1 <- polcom %>%
  mutate(therm_2 = therm_2 / 10,
    therm_1 = 10 - therm_1 / 10) %>%
  tidy_sem_model(news =~ news_1 + news_2 + news_3 + news_4 + news_5 + news_6,
    ambiv_sexism =~ ambiv_sexism_1 + ambiv_sexism_2 + ambiv_sexism_3 +
      ambiv_sexism_4 + ambiv_sexism_5 + ambiv_sexism_6,
    partisan =~ a*therm_1 + a*therm_2,
    ambiv_sexism ~ age + sex + hhinc + edu + news + partisan) %>%
  tidy_sem()
tidy_summary(sem1)

model <- model_


measurement_and_structural <- function(model) {
  ## if no regression equation, then they're the same
  if (grepl("(<==)\\s{0,}~", model_)) {
    return(list(measurement = model, structural = model))
  }
  ## store as structural model
  sm <- model
  ## drop regression lines for measurement model
  mm <- strsplit(model, "\\n")[[1]]
  mm <- grep("(?<==)\\s{0,}~", mm, perl = TRUE, invert = FALSE, value = TRUE)
  mm <- paste(mm, collapse = "\n")
  ## return model list
  list(measurement = mm, structural = sm)
}

measurement_and_structural(model)
update(sem1)

compare2cfa <- function(.data, model, robust = robust) {
  model <- measurement_and_structural(model)
  if (identical(model$measurement, model$structural)) {
    return(NULL)
  }
  if (robust) {
    estimator <- "mlr"
  } else {
    estimator <- "ml"
  }
  lavaan::cfa(model$measurement, data = .data, estimator = estimator)
}
