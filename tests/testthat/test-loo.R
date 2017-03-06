context("crossv_loo")

expect_crossv <- function(x) {
  expect_is(x, "data.frame")
  expect_named(x, c("train", "test", ".id"))
  expect_is(x$train, "list")
  expect_true(all(map_lgl(x$train, is.resample)))
  expect_is(x$test, "list")
  expect_true(all(map_lgl(x$test, is.resample)))
  expect_is(x$.id, "character")
}

local({

  dat <- tibble(a = c(1, 1, 2, 3))

  test_that("crossv_loo.data.frame works as expected", {
    x <- crossv_loo(dat)
    expect_crossv(x)
    expect_identical(nrow(x), nrow(dat))
    expect_identical(x$train, map(map(1:4, setdiff, x = 1:4),
                                  resample, data = dat))
    expect_identical(x$test, map(list(1L, 2L, 3L, 4L), resample, data = dat))
  })

  test_that("crossv_loo.grouped_df works as expected", {
    gdat <- group_by(dat, a)
    x <- crossv_loo(gdat)
    expect_crossv(x)
    expect_identical(nrow(x), n_groups(gdat))
    expect_identical(x$train, map(list(3:4, c(1:2, 4L), 1:3),
                                  resample, data = gdat))
    expect_identical(x$test, map(list(1:2, 3L, 4L), resample, data = gdat))
  })

  test_that("crossv_lpo.data.frame works as expected", {
    x <- crossv_lpo(dat, 2)
    expect_crossv(x)
    expect_identical(nrow(x), 6L)
    train_idx <- list(3:4, c(2L, 4L), 2:3, c(1L, 4L), c(1L, 3L), 1:2)
    test_idx <- list(1:2, c(1L, 3L), c(1L, 4L), 2:3, c(2L, 4L), 3:4)
    expect_identical(x$test, map(test_idx, resample, data = dat))
    expect_identical(x$train, map(train_idx, resample, data = dat))
  })

  test_that("crossv_lpo.grouped_df works as expected", {
    gdat <- group_by(dat, a)
    x <- crossv_lpo(gdat, 2)
    expect_crossv(x)
    expect_identical(nrow(x), 3L)
    train_idx <- list(4L, 3L, 1:2)
    test_idx <- list(1:3, c(1:2, 4L), 3:4)
    expect_identical(x$test, map(test_idx, resample, data = gdat))
    expect_identical(x$train, map(train_idx, resample, data = gdat))
  })


})
