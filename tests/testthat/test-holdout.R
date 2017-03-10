context("holdout_n.data.frame")

local({
  dat <- tibble(a = c(rep("a", 5), rep("b", 3), rep("c", 2)))
  dat_grouped <- group_by(dat, a)

  expect_holdout_df <- function(x, data = TRUE) {
    expect_is(x, "data.frame")
    expect_named(x, c("train", "test", ".id"))
    expect_is(x$train, "list")
    expect_is(x$test, "list")
    expect_is(x$.id, "integer")
    if (data) {
      expect_true(all(map_lgl(x$train, is.resample)))
      expect_true(all(map_lgl(x$test, is.resample)))
    }
  }

  test_that("holdout_n.data.frame works as expected", {
    n <- 2
    k <- 5
    x <- holdout_n(dat, n = n, k = k)
    expect_holdout_df(x)
    expect_equal(nrow(x), k)
    expect_true(all(map_int(x$test, ~ dim(.x)[1]) == n))
    expect_true(all(map_int(x$train, ~ dim(.x)[1]) == (nrow(dat) - n)))
  })

  test_that("holdout_n.grouped_df works as expected", {
    n <- 2
    k <- 5
    x <- holdout_n(dat_grouped, n = n, k = k, stratify = FALSE)
    expect_holdout_df(x)
    expect_equal(nrow(x), k)
  })

  test_that("holdout_n.grouped_df stratify = TRUE works as expected", {
    n <- 1
    k <- 5
    x <- holdout_n(dat_grouped, n = n, k = k, stratify = TRUE)
    expect_holdout_df(x)
    expect_equal(nrow(x), k)
  })

  test_that("holdout_n.default works as expected", {
    n <- 3
    k <- 5
    v <- 1:10
    x <- holdout_n(v, n = n, k = k)
    expect_holdout_df(x, data = FALSE)
    expect_equal(nrow(x), k)
    expect_true(all(map_int(x$test, length) == n))
    expect_true(all(map_int(x$train, length) == (length(v) - n)))
  })

})
