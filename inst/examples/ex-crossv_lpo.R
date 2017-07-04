library("purrr")

# Leave-one-out cross-validation
cv1 <- crossv_loo(mtcars)
models <- map(cv1$train, ~ lm(mpg ~ wt, data = .$sample))
summary(map2_dbl(models, map(cv1$test, ~ .$sample), modelr::rmse))

# Leave-two-out cross-validation
cv2 <- crossv_lpo(mtcars, 2)
models <- map(cv2$train, ~ lm(mpg ~ wt, data = .$sample))
summary(map2_dbl(models, map(cv2$test, ~ .$sample), modelr::rmse))

# Leave-p-out cross-validation using the number of obs
crossv_loo_n(100)
crossv_lpo_n(100, 2)
