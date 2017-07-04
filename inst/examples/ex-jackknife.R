require("dplyr")
require("purrr")

# generate jackknife datasets
jack <- jackknife(mtcars)
# get mean and sd of coefficients
map(jack$sample, ~ lm(mpg ~ wt, data = .$sample)) %>%
  map_df(broom::tidy) %>%
  group_by(term) %>%
  summarise_at(vars(estimate), funs(mean, sd))

# generate jackknife subsamples for a number of elements
jackknife_n(10)
