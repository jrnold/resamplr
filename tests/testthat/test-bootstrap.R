context("bootstrap")

test_that("bootstrap works with defaults and data frame", {
  x <- tibble(alpha = base::letters[1:4])
  bootstrap(x)
})

test_that("bootstrap.grouped_df", {



})