cv <- crossv_kfold(mtcars, 5)
cv

library("purrr")
models <- map(cv$train, ~ lm(mpg ~ wt, data = .))
errs <- map2_dbl(models, cv$test, modelr::rmse)
hist(errs)

# stratified cross-validation
library("dplyr")
cv2 <- crossv_kfold(group_by(mtcars, am), 5, stratify = TRUE)
cv2
models2 <- map(cv2$train, ~ lm(mpg ~ wt, data = .))
errs2 <- map2_dbl(models, cv2$test, modelr::rmse)
hist(errs2)
