context("resample")

test_that("resample with data frame data works", {
  foo <- tibble(a = 1:4)
  x <- resample(foo, 1:2)
  expect_is(x, "resample")
  expect_is(x, "resample_df")
  expect_identical(x[["idx"]], 1:2)
  expect_same_address(x[["data"]], foo)
  expect_identical(x[["data"]], foo)
})

test_that("resample with non-data frame data works", {
  foo <- 1:100
  x <- resample(foo, 1:2)
  expect_is(x, "resample")
  expect_identical(x[["idx"]], 1:2)
  expect_same_address(x[["data"]], foo)
  expect_identical(x[["data"]], foo)
})

test_that("resample.resample works as expected", {
  foo <- tibble(a = 1:10)
  idx1 <- 2:8
  idx2 <- 1:2
  x <- resample(resample(foo, idx1), idx2)
  expect_is(x, "resample")
  expect_identical(x[["idx"]], idx1[idx2])
  expect_same_address(x[["data"]], foo)
  expect_identical(x[["data"]], foo)
})

test_that("resample works with character indexes", {
  data <- list(a = 1, b = 3, c = "foo")
  idx <- c("a", "c")
  x <- resample(data, idx)
  expect_is(x, "resample")
  expect_identical(x[["idx"]], idx)
  expect_same_address(x[["data"]], data)
  expect_identical(x[["data"]], data)
})

test_that("resample works with numeric indexes", {
  data <- tibble(1:10)
  # people shouldn't give non-integer values ... but see if if still workds
  idx <- c(1.0, 5.1)
  x <- resample(data, idx)
  expect_is(x, "resample")
  expect_identical(x[["idx"]], as.integer(idx))
  expect_same_address(x[["data"]], data)
  expect_identical(x[["data"]], data)
})

test_that("resample throws error with invalid indexes", {
  data <- list(a = 1, b = 3, c = "foo")
  idx <- NULL
  expect_error(resample(data, idx),
               regexp = "idx is not an integer vector or idx is not a character vector")
  idx <- list(1, 2, 3)
  expect_error(resample(data, idx),
               regexp = "idx is not an integer vector or idx is not a character vector")
})

test_that("as.integer.resample returns indexes", {
  idx <- 2:3
  data <- tibble(a = 1:10)
  expect_identical(as.integer(resample(data, idx)), idx)
})

test_that("as.character.resample returns indexes", {
  idx <- c("a", "c")
  data <- list(a = 1, b = 2, c = 3)
  expect_identical(as.character(resample(data, idx)), idx)
})

test_that("collect.resample works with chracter indexes and non-data frames", {
  idx <- c("a", "c")
  data <- list(a = 1, b = 2, c = 3)
  expect_identical(collect(resample(data, idx)), list(a = 1, c = 3))
})

test_that("collect.resample_df works with data-frames", {
  data <- tibble(a = 1:10 * 2)
  idx <- c(1, 5)
  expect_identical(collect(resample(data, idx)),
                   tibble(a = c(2, 10)))
})

test_that("nrow.resample_df works as expected", {
  data <- tibble(a = 1:10)
  idx <- 1:3
  expect_identical(nrow(resample(data, idx)), 3L)
})

test_that("ncol.resample_df works as expected", {
  data <- tibble(a = 1:10, b = 1:10, c = 1:10, d = 1:10)
  idx <- 1:3
  expect_identical(ncol(resample(data, idx)), 4L)
})

test_that("c.resample works as expected", {
  data <- tibble(a = 1:10)
  x <- c(resample(data, 1:2), resample(data, 8:10))
  expect_identical(x, resample(data, c(1:2, 8:10)))
  expect_same_address(x$data, data)
})

test_that("c.resample works with one object", {
  data <- tibble(a = 1:10)
  foo <- resample(data, 1:3)
  expect_identical(foo, c(foo))
  expect_same_address(foo$data, data)
})

test_that("resample_lst works", {
  data <- tibble(a = 1:10)
  idx <- list(1:3, 5:8, c(1, 7))
  x <- resample_lst(data, idx)
  expect_identical(
    x, list(resample(data, idx[[1]]),
            resample(data, idx[[2]]),
            resample(data, idx[[3]]))
  )
  walk(map(x, "data"), expect_same_address, expected = data)
})
