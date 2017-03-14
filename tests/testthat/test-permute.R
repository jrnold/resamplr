context("permute")

local({
  dat <- tibble(a = rep(1:3, 4))
  gdat <- group_by(dat, a)

  test_that("permute.data.frame works as expected", {
    k <- 2
    x <- permute(dat, 2)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
  })

  test_that("permute.grouped_df works as expected", {
    k <- 2
    x <- permute(gdat, 2, stratify = FALSE)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
  })

  test_that("permute.grouped_df stratify = TRUE works as expected", {
    k <- 2
    x <- permute(gdat, 2, stratify = TRUE)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
  })

  test_that("permute returns full permutation if k > total permuations", {
    k <- 10
    dat <- tibble(a = 1:3)
    x <- permute(dat, k)
    expect_resample_df(x)
    expect_equal(nrow(x), factorial(nrow(dat)))
  })

})
