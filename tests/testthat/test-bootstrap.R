context("bootstrap")

expect_is_bootstrap <- function(x, n) {
  expect_is(x, "data.frame")
  expect_named(x, c("strap", ".id"))
  expect_is(x$strap, "list")
  expect_true(all(map_lgl(x$strap, is.resample)))
  expect_equal(nrow(x), n)
}

local({
  dat <- tibble(a = 1:4)

  test_that("resample_bootstrap.data.frame works as expected", {
    n <- 2
    set.seed(123456)
    expect_equal(resample_bootstrap(dat),
                 resample(dat, c(4, 4, 2, 2)))
    expect_equal(resample_bootstrap(dat),
                 resample(dat, c(2, 1, 3, 1)))
  })

  gdat <- group_by(tibble(a = rep(1:4, 2),
                          b = rep(c("a", "b", "c", "d"), each = 2)),
                   b)

  test_that("resample_bootstrap.grouped_df groups = TRUE, stratify = FALSE", {
    n <- 2
    set.seed(123456)
    expect_equal(
      as.data.frame(resample_bootstrap(gdat,
                                       groups = TRUE,
                                       stratify = FALSE)),
      tibble(a = as.integer(c(3, 4, 3, 4, 3, 4, 3, 4)),
             b = c("d", "d", "d", "d", "b", "b", "b", "b"))
    )
    expect_equal(
      as.data.frame(resample_bootstrap(gdat,
                                       groups = TRUE,
                                       stratify = FALSE)),
      tibble(a = c(3L, 4L, 1L, 2L, 1L, 2L, 1L, 2L),
             b = c("b", "b", "a", "a", "c", "c", "a", "a"))
    )
  })

  test_that("resample_bootstrap.grouped_df groups = TRUE, stratify = TRUE", {
    n <- 2
    set.seed(123456)
    expect_equal(
      as.data.frame(resample_bootstrap(gdat,
                                       groups = TRUE,
                                       stratify = TRUE)),
      tibble(a = c(3L, 3L, 4L, 3L, 4L, 3L, 4L, 4L),
             b = c("d", "d", "d", "d", "b", "b", "b", "b"))
    )
    expect_equal(
      as.data.frame(resample_bootstrap(gdat,
                                       groups = TRUE,
                                       stratify = TRUE)),
      tibble(a = c(4L, 3L, 3L, 4L, 3L, 3L, 3L, 3L),
             b = c("d", "d", "d", "d", "d", "d", "d", "d"))
    )
  })

  test_that("resample_bootstrap.grouped_df groups = FALSE, stratify = TRUE", {
    n <- 2
    set.seed(123456)
    expect_equal(
      as.data.frame(resample_bootstrap(gdat,
                                       groups = FALSE,
                                       stratify = TRUE)),
      tibble(a = c(2L, 2L, 3L, 3L, 1L, 1L, 4L, 3L),
            b = c("a", "a", "b", "b", "c", "c", "d", "d"))
    )
    expect_equal(
      as.data.frame(resample_bootstrap(gdat,
                                       groups = FALSE,
                                       stratify = TRUE)),
      tibble(a = c(2L, 1L, 4L, 4L, 2L, 2L, 4L, 4L),
             b = c("a", "a", "b", "b", "c", "c", "d", "d"))
    )
  })

  test_that("bootstrap works as expected", {
    n <- 2
    set.seed(123456)
    x <- bootstrap(dat, n)
    expect_is_bootstrap(x, n)
    expect_equal(map(x$strap, as.data.frame),
                 list(tibble(a = as.integer(c(4, 4, 2, 2))),
                      tibble(a = as.integer(c(2, 1, 3, 1)))))
  })

})
