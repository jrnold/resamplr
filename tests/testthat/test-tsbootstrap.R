context("tsbootstrap")

test_that("tsbootstrap.data.frame works as expected", {
  dat <- tibble(a = 1:6)
  R <- 2
  x <- tsbootstrap(dat, R = R, size = 2)
  expect_resample_df(x)
  expect_equal(nrow(x), R)
  expect_identical(map_int(x$sample, ~ length(as.integer(.x))),
                   rep(nrow(dat), R))
})

test_that("tsbootstrap.data.frame type = geom works as expected", {
  dat <- tibble(a = 1:6)
  R <- 2
  x <- tsbootstrap(dat, R = R, size = 2, type = "geom")
  expect_resample_df(x)
  expect_equal(nrow(x), R)
  expect_identical(map_int(x$sample, ~ length(as.integer(.x))),
                   rep(nrow(dat), R))
})

test_that("tsbootstrap.grouped_df works as expected", {
  dat <- group_by(tibble(a = rep(1:6, 2)), a)
  R <- 2
  size <- 3
  x <- tsbootstrap(dat, R = R, size = size)
  expect_resample_df(x)
  expect_equal(nrow(x), R)
  expect_identical(map_int(x$sample, ~ length(as.integer(.x))),
                   rep(nrow(dat), R))
})
