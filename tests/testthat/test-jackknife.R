context("jackknife")

test_that("resample_jackknife works as expected", {
  x <- tibble::tibble(a = 1:10)
  y <- resample_jackknife(x, 2)
  expect_is(y, "resample")
  expect_equal(nrow(y), nrow(x) - 1L)
  expect_equal(as.integer(y), setdiff(x$a, 2))
  expect_identical(x, y$data)
})

test_that("jackknife works as expected", {
  x <- tibble::tibble(a = 1:10)
  y <- jackknife(x)
  expect_is(y, "data.frame")
  expect_equal(nrow(y), nrow(x))
  expect_equal(ncol(y), 2)
  expect_named(y, c("jackknife", ".id"))
  expect_is(y$jackknife, "list")
  expect_is(y$.id, "integer")
  expect_true(all(purrr::map_lgl(y[["jackknife"]], inherits, "resample")))
  expect_true(all(purrr::map_int(y[["jackknife"]], nrow) == (nrow(x) - 1L)))
})
