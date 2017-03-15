# example from modelr::bootstrap
library("purrr")
library("dplyr")
boot <- bootstrap(mtcars, 100)

models <- map(boot$sample, ~ lm(mpg ~ wt, data = .))
tidied <- map_df(models, broom::tidy, .id = "id")

hist(subset(tidied, term == "wt")$estimate)
hist(subset(tidied, term == "(Intercept)")$estimate)
