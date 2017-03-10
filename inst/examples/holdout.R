# Example originally from modelr::crossv_mc
library("purrr")
library("dplyr")

# holdout three obs, repeat 10 times
cv1 <- holdout_n(mtcars, n = 3, k = 10)
models <- map(cv1$train, ~ lm(mpg ~ wt, data = .))
summary(map2_dbl(models, cv1$test, modelr::rmse))

# holdout two groups at a time in the test set
# repeat four times.
cv2 <- holdout_n(group_by(mtcars, cyl), n = 2, k = 4)
models <- map(cv2$train, ~ lm(mpg ~ wt, data = .))
summary(map2_dbl(models, cv2$test, modelr::rmse))

# stratified holdout
# holdout 1 obs each from each group. repeat 5 times.
cv2 <- holdout_n(group_by(mtcars, am), n = 1, k = 5, stratified = TRUE)
models <- map(cv2$train, ~ lm(mpg ~ wt, data = .))
summary(map2_dbl(models, cv2$test, modelr::rmse))
