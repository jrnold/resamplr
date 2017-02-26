context("crossv_loo")

test_that("crossv_loo works as expected", {
  dat <- tibble::tibble(x = 1:10)
  cv <- crossv_loo(dat)
  expect_is(cv, "data.frame")
  expect_equal(nrow(cv), nrow(dat))
  expect_equal(ncol(cv), 3L)
  expect_named(cv, c("train", "test", ".id"))
  expect_is(cv$train, "list")
  expect_is(cv$test, "list")
  expect_is(cv$.id, "character")
  expect_true(all(purrr::map_lgl(cv[["train"]], inherits, "resample")))
  expect_true(all(purrr::map_lgl(cv[["test"]], inherits, "resample")))
  expect_true(all(purrr::map_int(cv[["train"]], nrow) == (nrow(cv) - 1L)))
  expect_true(all(purrr::map_int(cv[["test"]], nrow) == 1L))
  expect_equivalent(sort(purrr::map_int(cv[["test"]], as.integer)),
                    seq_len(nrow(cv)))

})