context("resample")

local({
  dat <- tibble(a = 1:5)

  test_that("resample_lst works", {
    expect_is(resample_lst(dat, list(1:2, 3:4)), "list")
  })

})
