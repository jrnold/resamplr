context("crossv_kfold")

test_that("crossv_kfold.grouped_df works as expected", {
  dat <- tibble::tibble(x = 1:6)
  cv <- crossv_kfold(dat, 3)
  expect_is(cv, "data.frame")
  expect_named(cv, c("train", "test", ".id"))
  expect_equal(nrow(cv), 3)
  idx <- map2(cv$train, cv$test, ~ c(as.integer(.x), as.integer(.y)))
  expect_true(all(map_int(idx, ~ length(setdiff(1:6, .x))) == 0))
  expect_true(all(map_int(map(cv$train, as.integer), length) == 4))
  expect_true(all(map_int(map(cv$test, as.integer), length) == 2))
  expect_equal(cv$.id, as.character(1:3))
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

