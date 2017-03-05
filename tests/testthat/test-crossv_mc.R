context("crossv_mc")
{
  dat <- tibble(a = 1:4)

  expect_crossv_mc <- function(x, n) {
    expect_is(x, "data.frame")
    expect_named(x, c("train", "test", ".id"))
    expect_equal(nrow(x), n)
    expect_is(x$train, "list")
    expect_true(all(map_lgl(x$train, is.resample)))
    expect_is(x$test, "list")
    expect_true(all(map_lgl(x$test, is.resample)))
    expect_is(x$.id, "character")
  }

  test_that("crossv_mc works as expected", {
    n <- 2
    set.seed(1)
    x <- crossv_mc(dat, n = n, test = 0.2)
    expect_crossv_mc(x, n)
    # results checked by hand
    expect_equal(map(x$train, as.integer),
                 map(list(c(2, 4, 3), c(1, 3, 2)), as.integer))
    expect_equal(map(x$test, as.integer),
                 map(list(1, 4), as.integer))
  })

  test_that("crossv_mc.grouped_df stratify = FALSE works as expected", {
    n <- 2
    dat <- group_by(tibble(a = rep(1:4, 4)), a)
    set.seed(12345)
    x <- crossv_mc(dat, n = n, test = 0.25, stratify = FALSE)
    expect_crossv_mc(x, n)
    # results checked by hand
    expect_equal(map(x$test, as.data.frame),
                 list(tibble(a = rep(1L, 4)), tibble(a = rep(4L, 4))))
    expect_equal(map(x$train, as.data.frame),
                 list(tibble(a = rep(c(3L, 4L, 2L), each = 4)),
                      tibble(a = rep(c(2L, 1L, 3L), each = 4))))
  })

  test_that("crossv_mc.grouped_df stratify = TRUE works as expected", {
    n <- 2
    dat <- group_by(tibble(a = rep(1:4, 4)), a)
    x <- crossv_mc(dat, n = n, test = 0.25, stratify = TRUE)
    expect_crossv_mc(x, n)
    # given the data, should always be true
    expect_equal(map(x$test, as.data.frame),
                 list(tibble(a = 1:4), tibble(a = 1:4)))
    expect_equal(map(x$train, as.data.frame),
                 list(tibble(a = rep(1:4, each = 3)),
                      tibble(a = rep(1:4, each = 3))))
  })

}
