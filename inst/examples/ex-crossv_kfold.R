# Example originally from modelr::crossv_mc
library("purrr")
library("dplyr")

# 5-fold cross-validation
cv1 <- crossv_kfold(mtcars, k = 5)
models <- map(cv1$train, ~ lm(mpg ~ wt, data = .))
summary(map2_dbl(models, cv1$test, modelr::rmse))

# k-fold by group
cv2 <- crossv_kfold(group_by(mtcars, cyl), k = 2)
models <- map(cv2$train, ~ lm(mpg ~ wt, data = .))
summary(map2_dbl(models, cv2$test, modelr::rmse))

# stratified k-fold
cv3 <- crossv_kfold(group_by(mtcars, am), k = 4, stratified = TRUE)
models <- map(cv3$train, ~ lm(mpg ~ wt, data = .))
summary(map2_dbl(models, cv3$test, modelr::rmse))
