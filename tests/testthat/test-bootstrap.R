context("bootstrap")

test_that("bootstrap works with defaults and data frame", {
  x <- tibble(alpha = base::letters[1:4])
  bootstrap(x)
})

test_that("bootstrap.grouped_df", {



})

debug(resamplr:::bootstrap_groups_1)
debug(resamplr:::bootstrap_groups_)
resamplr:::bootstrap_groups_(attr(group_by(mtcars, cyl), "indices"), R = 10)
