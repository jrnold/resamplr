context("resample_idx")

test_that("holdout_idx.default works as expected data frame", {
  dat <- tibble(a = letters[1:10])
  train <- list(1:8, c(4:8))
  # purposefully choose values that don't cover all
  test <- list(9:10, c(1:3))
  x <- holdout_idx(dat, train, test)
  expected <- tibble(train = resample_lst(dat, train),
                     test = resample_lst(dat, test),
                     .id = seq_along(train))
  expect_all_same_address(map(x$train, "data"), dat)
  expect_all_same_address(map(x$test, "data"), dat)
})

test_that("holdout_idx.default works as expected with non-data frame", {
  dat <- letters[1:10]
  train <- list(1:8, c(4:8))
  # purposefully choose values that don't cover all
  test <- list(9:10, c(1:3))
  x <- holdout_idx(dat, train, test)
  expected <- tibble(train = resample_lst(dat, train),
                     test = resample_lst(dat, test),
                     .id = seq_along(train))
  expect_all_same_address(map(x$train, "data"), dat)
  expect_all_same_address(map(x$test, "data"), dat)
})

test_that("resample_idx.grouped_df works as expected", {
  dat <- group_by(tibble(a = rep(1:3, 2)), a)
  train <- list(1L, 2:3)
  test <- list(2, 1)
  x <- holdout_idx(dat, train, test)
  expected <- tibble(train = resample_lst(dat, list(c(1,4), c(2, 5, 3, 6))),
                     test = resample_lst(dat, list(c(2, 5), c(1, 4))),
                     .id = seq_along(train))
  expect_all_same_address(map(x$train, "data"), dat)
  expect_all_same_address(map(x$test, "data"), dat)
})

test_that("holout_idx.resample works as expected", {
  dat <- tibble(alpha = letters[1:10])
  idx <- 2:8
  train <- list(1:4, 5:7)
  test <- list(5:7, 1:4)
  x <- holdout_idx(resample(dat, idx), train, test)
  expected <- tibble(train =
                       resample_lst(dat, list(2:5, 6:8)),
                     test =
                       resample_lst(dat, list(6:8, 2:5)),
                     .id = seq_along(train))
  expect_all_same_address(map(x$train, "data"), dat)
  expect_all_same_address(map(x$test, "data"), dat)
})
