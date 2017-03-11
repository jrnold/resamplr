library(tibble)
library(modelr)
library(dplyr)
library(purrr)

expect_crossv_df <- function(x, data = TRUE) {
  expect_is(x, "data.frame")
  expect_named(x, c("train", "test", ".id"))
  expect_is(x$train, "list")
  expect_is(x$test, "list")
  expect_is(x$.id, "integer")
  if (data) {
    expect_true(all(map_lgl(x$train, is.resample)))
    expect_true(all(map_lgl(x$test, is.resample)))
  }
}
