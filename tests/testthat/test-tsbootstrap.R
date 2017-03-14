context("tsbootstrap")

test_that("tsbootstrap.data.frame works as expected", {
  dat <- tibble(a = 1:6)
  k <- 2
  x <- tsbootstrap(dat, k = k, size = 2)
  expect_resample_df(x)
  expect_equal(nrow(x), k)
  expect_identical(map_int(x$sample, ~ length(as.integer(.x))),
                   rep(nrow(dat), k))
})

test_that("tsbootstrap.grouped_df works as expected", {
  dat <- group_by(tibble(a = rep(1:6, 2)), a)
  k <- 2
  size <- 3
  x <- tsbootstrap(dat, k = k, size = size)
  expect_resample_df(x)
  expect_equal(nrow(x), k)
  expect_identical(map_int(x$sample, ~ length(as.integer(.x))),
                   rep(nrow(dat), k))
})