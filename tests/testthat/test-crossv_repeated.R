context("crossv_repeated")

test_that("crossv_repeated works as expected", {
  x <- tibble::tibble(foo = 1:10)
  n <- 3
  k <- 5
  y <- crossv_repeated(x, k = k, n = n)
  expect_is(y, "data.frame")
  expect_equal(nrow(y), n * k)
  expect_equal(ncol(y), 3)
  expect_named(y, c("train", "test", ".id"))
  expect_is(y$test, "list")
  expect_is(y$train, "list")
  expect_is(y$.id, "character")
  expect_true(all(purrr::map_lgl(y[["test"]], inherits, "resample")))
  expect_true(all(purrr::map_int(y[["test"]], nrow) == nrow(x) %/% k))
  expect_true(all(purrr::map_lgl(y[["train"]], inherits, "resample")))
  expect_true(all(purrr::map_int(y[["train"]], nrow) ==
                    (nrow(x) %/% k) * (k - 1)))
})
