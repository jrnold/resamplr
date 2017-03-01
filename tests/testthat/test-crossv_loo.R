context("crossv_loo")

test_that("crossv_loo.grouped_df works as expected", {
  dat <- tibble::tibble(x = 1:3)
  cv <- crossv_loo(dat)
  expected <- tibble(
    train = map(list(c(2L, 3L), c(1L, 3L), c(1L, 2L)), resample, data = dat),
    test = map(1:3, resample, data = dat),
    .id = as.character(1:3)
  )
  expect_identical(cv, expected)
})

test_that("crossv_loo.grouped_df works as expected", {
  dat <- tibble::tibble(foo = c("a", "a", "b", c("c", "c")),
                        bar = 1:5) %>%
    group_by(foo)
  cv <- crossv_loo(dat)
  expected <- tibble(
    train = map(list(3:5, c(1:2, 4:5), 1:3), resample, data = dat),
    test = map(list(1:2, 3L, 4:5), resample, data = dat),
    .id = as.character(1:3)
  )
  expect_identical(cv$train, expected$train)
  expect_identical(cv$test, expected$test)
  expect_identical(cv$.id, expected$.id)
})

test_that("crossv_lpo.grouped_df works as expected", {
  dat <- tibble::tibble(x = 1:3)
  cv <- crossv_lpo(dat, 2)
  expected <- tibble(
    train = map(list(3L, 2L, 1L), resample, data = dat),
    test = map(list(1:2, c(1L, 3L), 2:3), resample, data = dat),
    .id = as.character(1:3)
  )
  expect_identical(cv$train, expected$train)
  expect_identical(cv$test, expected$test)
  expect_identical(cv$.id, expected$.id)
})

test_that("crossv_lpo.grouped_df works as expected", {
  dat <- tibble::tibble(foo = c("a", "a", "b", "c", "c"),
                        bar = 1:5) %>%
    group_by(foo)
  cv <- crossv_lpo(dat, 2)
  expected <- tibble(
    train = map(list(4:5, 3L, 1:2), resample, data = dat),
    test = map(list(1:3, c(1:2, 4:5), c(3:5)), resample, data = dat),
    .id = as.character(1:3)
  )
  expect_identical(cv$train, expected$train)
  expect_identical(cv$test, expected$test)
  expect_identical(cv$.id, expected$.id)
})