context("resmple_holdout")

test_that("resample_holdout.data.frame throws error with two NULL args", {
  expect_error(resample_holdout(tibble(a = 1:10)))
  expect_error(resample_holdout(tibble(a = 1:10), test = NULL, train = NULL))
})

test_that("resample_holdout.data.frame works with non-null test", {
  dat <- tibble(a = 1:3)
  expect_equal(resample_holdout(dat, test = 1L),
               list(train = resample(dat, 2:3), test = resample(dat, 1L)))
})

test_that("resample_holdout.data.frame works with non-null train", {
  dat <- tibble(a = 1:3)
  expect_equal(resample_holdout(dat, train = 2L),
               list(train = resample(dat, 2L),
                    test = resample(dat, c(1L, 3L))))
})

test_that("resample_holdout.data.frame works with non-null train and test", {
  dat <- tibble(a = 1:3)
  expect_equal(resample_holdout(dat, train = 2L, test = 3L),
               list(train = resample(dat, 2L),
                    test = resample(dat, 3L)))
})

# grouped_df

test_that("resample_holdout.grouped_df works with non-null train and test", {
  dat <- group_by(tibble(a = c(1, 1, 3, 3, 10)), a)
  expect_equal(resample_holdout(dat, train = 2L, test = 3L),
               list(train = resample(dat, 3:4),
                    test = resample(dat, 5L)))
})

test_that("resample_holdout.grouped_df works with non-null train", {
  dat <- group_by(tibble(a = c(1, 1, 3, 3, 10)), a)
  expect_equal(resample_holdout(dat, train = 2L),
               list(train = resample(dat, 3:4),
                    test = resample(dat, c(1:2, 5L))))
})

test_that("resample_holdout.grouped_df works with non-null test", {
  dat <- group_by(tibble(a = c(1, 1, 3, 3, 10)), a)
  expect_equal(resample_holdout(dat, test = 2L),
               list(train = resample(dat, c(1:2, 5L)),
                    test = resample(dat, 3:4)))
})

test_that("resample_holdout.grouped_df throws error with NULL train and test", {
  dat <- group_by(tibble(a = c(1, 1, 3, 3, 10)), a)
  expect_error(resample_holdout(dat))
  expect_error(resample_holdout(dat, test = NULL, train = NULL))
})