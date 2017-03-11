context("jackknife")

expect_resample_df <- function(x, data = TRUE) {
  expect_is(x, "data.frame")
  expect_is(x$sample, "list")
  expect_is(x$.id, "integer")
  if (data) {
    expect_true(all(map_lgl(x$sample, is.resample)))
  }
}

local({
  dat <- tibble(a = rep(1:3, 2))

  test_that("jackknife.data.frame works as expected", {
    out <- jackknife(dat)
    expect_resample_df(out)
    expect_equal(nrow(dat), nrow(out))
    expect_true(all(map_int(out$sample, ~ dim(.x)[1]) == (nrow(dat) - 1)))
  })

  test_that("jackknife.data.frame p=2 works as expected", {
    p <- 2
    out <- jackknife(dat, p = p)
    expect_resample_df(out)
    expect_equal(choose(nrow(dat), p), nrow(out))
    expect_true(all(map_int(out$sample, ~ dim(.x)[1]) == (nrow(dat) - p)))
  })

  test_that("jackknife.grouped_df works as expected", {
    p <- 1
    grouped_dat <- group_by(dat, a)
    out <- jackknife(grouped_dat, p = p)
    expect_resample_df(out)
    expect_equal(n_groups(grouped_dat), nrow(out))
    # hardcoded from the example
    expect_true(all(map_int(out$sample, ~ dim(.x)[1]) == 4))
  })
})
