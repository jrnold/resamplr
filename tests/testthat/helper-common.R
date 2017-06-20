library("tibble")
library("modelr")
library("dplyr")
library("purrr")

make_label <- function(object, label = NULL) {
  label %||% rlang::expr_label(substitute(object))
}

expect_resample_lst <- function(x) {
  expect_true(is_resample_lst(x))
}

expect_crossv_df <- function(x, test = NULL, train = NULL) {
  expect_is(x, "data.frame")
  expect_named(x, c("train", "test", ".id"))
  expect_is(x$train, "list")
  expect_is(x$test, "list")
  expect_is(x$.id, "integer")
  expect_resample_lst(x[["train"]])
  expect_resample_lst(x[["test"]])
  if (!is.null(test)) {
    expect_identical(map(x$test, as.integer), test)
  }
  if (!is.null(train)) {
    expect_identical(map(x$train, as.integer), train)
  }
}

expect_resample_dataframe <- function(x, expected = NULL) {
  expect_is(x, "data.frame")
  expect_is(x$sample, "list")
  expect_is(x$.id, "integer")
  expect_resample_lst(x[["sample"]])
  if (!is.null(expected)) {
    expect_identical(map(x$sample, as.integer), expected)
  }
}

expect_same_address <- function(object, expected, info = NULL, label = NULL, expected.label = NULL) {
  lab_obj <- make_label(object, label)
  lab_exp <- make_label(expected, expected.label)
  addr_obj <- pryr::inspect(object)$address
  addr_exp <- pryr::inspect(expected)$address
  expect(addr_obj == addr_exp,
         sprintf("%s and %s have different addresses. %s, %s",
                 lab_obj, lab_exp, addr_obj, addr_exp))
  invisible(object)
}

