context("resample")

local({
  dat <- tibble(a = 1:5)
  test_that("resample works as expected", {
    idx <- 2:3
    x <- resample(dat, idx)
    expect_is(x, "resample")
    expect_identical(x$data, dat)
    expect_identical(x$idx, idx)
  })

  test_that("resample works with numeric idx", {
    idx <- c(2, 3)
    expect_is(resample(dat, idx), "resample")
  })

  test_that("resample throws error with non-numeric idx", {
    msg <- "idx is not a numeric or integer vector"
    expect_error(resample(dat, c("1", "2")), regexp = msg)
  })

  test_that("resample throws error if data is not a data frame",  {
    expect_error(resample(1:3, 1:3), regexp = "data is not a data frame")
  })

  test_that("resample throws errors if idx is missing",  {
    expect_error(resample(dat, c(1L, NA_integer_)),
                 regexp = "Elements .* of !is.na\\(idx\\) are not true")
  })

  test_that("resample throws errors if idx outside",  {
    msg <- "Elements .* of idx .* are not true"
    expect_error(resample(dat, c(0L, 1:2)), regexp = msg)
    expect_error(resample(dat, c(1L, nrow(dat) + 1L)), regexp = msg)
  })

  test_that("resample_lst works", {
    expect_is(resample_lst(dat, list(1:2, 3:4)), "list")
  })

})
