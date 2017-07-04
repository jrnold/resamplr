library("purrr")
library("dplyr")
library("ggplot2")

boot <- bootstrap(mtcars, 100)

models <- map(boot$sample, ~ lm(mpg ~ wt, data = .$sample))
tidied <- map_df(models, broom::tidy, .id = "id")

ggplot(tidied, aes(x = estimate)) +
  geom_histogram() +
  facet_wrap(~ term, scales = "free_x")

# bootstrap_n requires only a number of elements
bootstrap_n(20, 10)

# Specify weights for a weighted bootstrap
bootstrap(mtcars, 10, weights = runif(nrow(mtcars)))

# A Bayesian bootstrap
bootstrap(mtcars, 10, bayesian = TRUE)

# Subsample subsets of size 5 without replacement
bootstrap(mtcars, 10, size = 5, replace = FALSE)

# Subsample subsets of 25% without replacement
bootstrap(mtcars, 10, frac = 0.25, replace = FALSE)
