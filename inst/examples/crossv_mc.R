# Example originally from modelr::crossv_mc
cv <- crossv_mc(mtcars, 20)
cv

library("purrr")
models <- map(cv$train, ~ lm(mpg ~ wt, data = .))
errs <- map2_dbl(models, cv$test, modelr::rmse)
hist(errs)

# stratified random partitions
library("dplyr")
cv2 <- crossv_mc(group_by(mtcars, am), 20, stratify = TRUE)
cv2
models2 <- map(cv2$train, ~ lm(mpg ~ wt, data = .))
errs2 <- map2_dbl(models, cv2$test, modelr::rmse)
hist(errs2)
