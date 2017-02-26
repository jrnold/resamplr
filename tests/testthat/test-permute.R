context("permute")

test_that("resample_permute works as expected", {
  x <- tibble::tibble(foo = 1:10)
  y <- resample_permutation(x)
  expect_is(y, "resample")
  expect_identical(dim(y), dim(x))
  expect_identical(y$data, x)
  expect_identical(sort(as.integer(y)), seq_len(nrow(x)))
})

test_that("resample_permute works as expected", {
  x <- tibble::tibble(foo = 1:10)
  n <- 3
  y <- permute(x, n)
  expect_is(y, "data.frame")
  expect_equal(nrow(y), n)
  expect_equal(ncol(y), 2)
  expect_named(y, c("perm", ".id"))
  expect_is(y$perm, "list")
  expect_is(y$.id, "character")
  expect_true(all(purrr::map_lgl(y[["perm"]], inherits, "resample")))
  expect_true(all(purrr::map_int(y[["perm"]], nrow) == (nrow(x))))
})
