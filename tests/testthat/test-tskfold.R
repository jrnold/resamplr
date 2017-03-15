context("crossv_tskfold")

local({
  dat <- tibble(a = rep(1:4, each = 2))
  gdat <- group_by(dat, a)

  test_that("crossv_tskfold.data.frame works as expected", {
    K <- 4
    x <- crossv_tskfold(dat, K)
    expect_crossv_df(x)
    expect_equal(nrow(x), K - 1)
  })

  test_that("crossv_tskfold.grouped_df works as expected", {
    K <- 4
    x <- crossv_tskfold(gdat, K)
    expect_crossv_df(x)
    expect_equal(nrow(x), K - 1)
  })
})
