context("resample")

local({
  dat <- tibble(a = 1:5)

  test_that("resample works", {
    x <- resample(dat, 1:2)
    expect_is(x, "resample")
    expect_identical(x[["data"]], dat)
    expect_identical(x[["idx"]], 1:2)
  })

  test_that("resample_lst works", {
    expect_identical(resample_lst(dat, list(1:2, 3:4)),
                     list(resample(dat, 1:2), resample(dat, 3:4)))
  })

})
