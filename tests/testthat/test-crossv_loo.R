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
  dat <- tibble::tibble(foo = c("a", "a", "b"), bar = 1:3) %>%
    group_by(foo)
  cv <- crossv_loo(dat)
  expected <- tibble(
    train = map(list(c(3L), c(1L, 2L)), resample, data = dat),
    test = map(list(1:2, 3L), resample, data = dat),
    .id = as.character(1:2)
  )
  expect_identical(cv, expected)
})