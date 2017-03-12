context("crossv_tskfold")

local({
  dat <- tibble(a = rep(1:4, each = 2))
  gdat <- group_by(dat, a)

  test_that("crossv_tskfold.data.frame works as expected", {
    k <- 4
    x <- crossv_tskfold(dat, k)
    expect_crossv_df(x)
    expect_equal(nrow(x), k - 1)
  })

  test_that("crossv_tskfold.grouped_df works as expected", {
    k <- 4
    x <- crossv_tskfold(gdat, k)
    expect_crossv_df(x)
    expect_equal(nrow(x), k - 1)
  })
})
