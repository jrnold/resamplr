context("crossv_tskfold")

test_that("crossv_tskfold.data.frame works as expected", {
  dat <- tibble::tibble(x = 1:6)
  cv <- crossv_kfold(dat, 3)
  expected <-
    tibble(
      train = resample_list(dat, list(4:5,))
    )
})

test_that("crossv_kfold.grouped_df works as expected", {
  dat <- tibble::tibble(foo = c("a", "a", "b", c("c", "c")),
                        bar = 1:5) %>%
    group_by(foo)
  cv <- crossv_kfold(dat, 3)
  expect_is(cv, "data.frame")
  expect_named(cv, c("train", "test", ".id"))
  expect_equal(nrow(cv), 3)
  idx <- map2(cv$train, cv$test, ~ c(as.integer(.x), as.integer(.y)))
  expect_true(all(map_int(idx, ~ length(setdiff(1:5, .x))) == 0))
  expect_equal(cv$.id, as.character(1:3))
})

test_that("crossv_kfold.grouped_df throws error with non-single integer", {
  msg <- "k is not a number"
  x <- group_by(tibble(a = 1:5), a)
  expect_error(crossv_kfold(x, k = 1:2), regexp = msg)
  expect_error(crossv_kfold(x, k = NULL), regexp = msg)
  expect_error(crossv_kfold(x, k = "12"), regexp = msg)
})
