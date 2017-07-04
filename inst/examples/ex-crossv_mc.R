library("purrr")
library("dplyr")

# 5-fold cross-validation
cv1 <- crossv_mc(mtcars)
models <- map(cv1$train, ~ lm(mpg ~ wt, data = .$sample))
summary(map2_dbl(models, map(cv1$test, ~ .$sample), modelr::rmse))

# crossv_kfold_n works with a number of elements
cv2 <- crossv_mc_n(100)
cv2
cv2$train[[1]]
cv2$test[[2]]
