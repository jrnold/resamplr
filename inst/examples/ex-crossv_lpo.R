# Example originally from modelr::crossv_mc
library("purrr")
library("dplyr")

# LOO cross-validation
cv1 <- crossv_loo(mtcars)
models <- map(cv1$train, ~ lm(mpg ~ wt, data = .))
summary(map2_dbl(models, cv1$test, modelr::rmse))

# Leave-p-Out cross-validation with p = 2
cv2 <- crossv_lpo(mtcars, p = 2)
models <- map(cv2$train, ~ lm(mpg ~ wt, data = .))
summary(map2_dbl(models, cv2$test, modelr::rmse))
