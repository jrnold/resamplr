context("crossv_df")

test_that("crossv_df.data.frame with test works as expected", {
  dat <- tibble(a = 1:6)
  test <- list(1:2, 2:3)
  expect_identical(crossv_df(dat, test = test),
                   tibble(train = resample_lst(dat, list(3:6, c(1L, 4:6))),
                          test = resample_lst(dat, list(1:2, 2:3)),
                          .id = 1:2))
})

test_that("crossv_df.data.frame with train works as expected", {
  dat <- tibble(a = 1:6)
  train <- list(1:2, 2:3)
  expect_identical(crossv_df(dat, train = train),
                   tibble(train = resample_lst(dat, list(1:2, 2:3)),
                          test = resample_lst(dat, list(3:6, c(1L, 4:6))),
                          .id = 1:2))
})

test_that("crossv_df.data.frame with train works as expected", {
  dat <- tibble(a = 1:6)
  test <- list(1:2, 2:3)
  train <- list(1:5, 4:6)
  expect_identical(crossv_df(dat, train = train, test = test),
                   tibble(train = resample_lst(dat, train),
                          test = resample_lst(dat, test),
                          .id = 1:2))
})

test_that("crossv_df.data.frame with null train and test throws an error", {
  expect_error(crossv_df(dat))
})

test_that("crossv_df.grouped_df with test works as expected", {
  dat <- group_by(tibble(a = rep(1:3, 2)), a)
  test <- list(1L, 2:3)
  expect_identical(crossv_df(dat, test = test),
                   tibble(
                     train = resample_lst(dat,
                                          list(c(2L, 5L, 3L, 6L),
                                               c(1L, 4L))),
                     test = resample_lst(dat,
                                         list(c(1L, 4L),
                                              c(2L, 5L, 3L, 6L))),
                     .id = seq_along(test))
                   )
})

test_that("crossv_df.grouped_df with train works as expected", {
  dat <- group_by(tibble(a = rep(1:3, 2)), a)
  train <- list(1L, 2:3)
  expect_identical(crossv_df(dat, train = train),
                   tibble(
                     train = resample_lst(dat,
                                          list(c(1L, 4L),
                                              c(2L, 5L, 3L, 6L))),
                     test =  resample_lst(dat,
                                          list(c(2L, 5L, 3L, 6L),
                                               c(1L, 4L))),
                     .id = seq_along(test))
  )
})

test_that("crossv_df.grouped_df with train and test works as expected", {
  dat <- group_by(tibble(a = rep(1:3, 2)), a)
  train <- list(1L, 2:3)
  test <- list(2L, 1L)
  expect_identical(crossv_df(dat, test = test, train = train),
                   tibble(
                     train = resample_lst(dat,
                                          list(c(1L, 4L),
                                               c(2L, 5L, 3L, 6L))),
                     test = resample_lst(dat,
                                         list(c(2L, 5L), c(1L, 4L))),
                     .id = seq_along(test)))
})
