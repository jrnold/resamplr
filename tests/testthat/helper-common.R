library(tibble)
library(modelr)
library(dplyr)
library(purrr)

expect_crossv_df <- function(x, test = NULL, train = NULL) {
  expect_is(x, "data.frame")
  expect_named(x, c("train", "test", ".id"))
  expect_is(x$train, "list")
  expect_is(x$test, "list")
  expect_is(x$.id, "integer")
  expect_true(all(map_lgl(x$train, is.resample)))
  expect_true(all(map_lgl(x$test, is.resample)))
  if (!is.null(test)) {
    expect_identical(map(x$test, as.integer), test)
  }
  if (!is.null(train)) {
    expect_identical(map(x$train, as.integer), train)
  }
}

expect_resample_df <- function(x, expected = NULL) {
  expect_is(x, "data.frame")
  expect_is(x$sample, "list")
  expect_is(x$.id, "integer")
  expect_true(all(map_lgl(x$sample, is.resample)))
  if (!is.null(expected)) {
    expect_identical(map(x$sample, as.integer), expected)
  }
}
