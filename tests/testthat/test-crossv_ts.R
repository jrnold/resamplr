context("crossv_ts")

test_that("crossv_ts.data.frame works as expected", {
  dat <- tibble(1:4)
  expect_crossv_df(crossv_ts(dat), train = list(1L, 1:2, 1:3),
                   test = list(2L, 3L, 4L))
  expect_crossv_df(crossv_ts(dat, horizon = 2L),
                   train = list(1L, 1:2), test = list(3L, 4L))
  expect_crossv_df(crossv_ts(dat, test_size = 2L),
                   train = list(1L, 1:2), test = list(2:3, 3:4))
  expect_crossv_df(crossv_ts(dat, train_size = 1L),
                   train = list(1L, 2L, 3L), test = list(2L, 3L, 4L))
  expect_crossv_df(crossv_ts(dat, train_size = 2L, train_partial = FALSE),
                   train = list(1:2, 2:3), test = list(3L, 4L))
  expect_crossv_df(crossv_ts(dat, test_size = 2L, test_partial = FALSE),
                   train = list(1L, 1:2), test = list(2:3, 3:4))
  expect_crossv_df(crossv_ts(tibble(a = 1:10), from = 2, to = 8, by = 2),
                   train = list(1L, 1:3, 1:5, 1:7),
                   test = list(2L, 4L, 6L, 8L))
  expect_crossv_df(crossv_ts(tibble(a = 1:10), test_start = c(5L, 9L)),
                   train = list(1:4, 1:8), test = list(5L, 9L))
})

test_that("crossv_ts.grouped_df works as expected", {
  dat <- group_by(tibble(a = rep(1:4, each = 2)), a)
  expect_crossv_df(crossv_ts(dat),
                   train = list(1:2, 1:4, 1:6),
                   test = list(3:4, 5:6, 7:8))
})
