context("bootstrap")

local({
  dat <- tibble(a = rep(1:3, 4))
  grouped_dat <- group_by(dat, a)

  test_that("bootstrap works as expected", {
    x <- bootstrap(dat, 2)
    expect_resample_df(x)
  })



})