context("resmple_holdout")

{
  dat <- tibble(a = c(rep("a", 5), rep("b", 3), rep("c", 2)))
  dat_grouped <- group_by(dat, a)

  expect_holdout <- function(x) {
    expect_is(x, "list")
    expect_named(x, c("train", "test"))
    expect_identical(map_chr(x, class),
                     c(train = "resample", test = "resample"))
  }

  test_that("resample_holdout.data.frame works as expected", {
    x <- resample_holdout(dat)
    expect_holdout(x)
    expect_identical(map(x, dim),
                     list(train = c(7L, 1L), test = c(3L, 1L)))
  })

  test_that("resample_holdout.data.frame works with test arg", {
    x <- resample_holdout(dat, test = 0.1)
    expect_holdout(x)
    expect_identical(map(x, dim),
                     list(train = c(9L, 1L), test = c(1L, 1L)))
  })

  test_that("resample_holdout.grouped_df works as expected", {
    x <- resample_holdout(dat_grouped)
    expect_holdout(x)
  })

  test_that("resample_holdout.grouped_df works with stratify", {
    x <- resample_holdout(dat_grouped, stratify = TRUE)
    expect_holdout(x)
  })

  test_that("resample_holdout_n.data.frame works as expected", {
    x <- resample_holdout_n(dat)
    expect_holdout(x)
    expect_identical(map(x, dim),
                     list(train = c(9L, 1L), test = c(1L, 1L)))
  })

  test_that("resample_holdout_n.data.frame works with test = NULL", {
    x <- resample_holdout_n(dat, test = 2L)
    expect_holdout(x)
    expect_identical(map(x, dim),
                     list(train = c(8L, 1L), test = c(2L, 1L)))
  })

  # ---- resample_holdout_n

  test_that("resample_holdout_n.grouped_df works as expected", {
    x <- resample_holdout_n(dat_grouped)
    expect_holdout(x)
  })

  test_that("resample_holdout_n.grouped_df works with stratify = TRUE", {
    x <- resample_holdout_n(dat_grouped, test = 1L, stratify = TRUE)
    expect_holdout(x)
  })

  test_that("resample_holdout_n.grouped_df works as expected", {
    x <- resample_holdout_n(dat_grouped, test = 2L, stratify = TRUE)
    expect_holdout(x)
  })

}