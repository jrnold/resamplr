context("utils")

test_that("is.resample works as expected", {
  expect_true(is.resample(resample(tibble(a = 1:10), 1L)))
  expect_false(is.resample(TRUE))
})

test_that("c.resample works as expected", {
  dat <- tibble(a = 1:10)
  expect_identical(c(resample(dat, 1:2), resample(dat, 8:10)),
                   resample(dat, c(1:2, 8:10)))
})

test_that("c.resample works with one object", {
  foo <- resample(tibble(a = 1:10), 1:3)
  expect_identical(foo, c(foo))
})

test_that("c.resample throws error with a non-resample object", {
  expect_error(c(resample(tibble(a = 1:10), 1:3), 5:10),
               regexp = "All objects must inherit from class `resample`")
})

test_that("c.resample throws error with non-identical data", {
  expect_error(c(resample(tibble(a = 1:5), 1:3),
                 resample(tibble(b = 1:10), 1:4)),
               regexp = "identical data")

})
