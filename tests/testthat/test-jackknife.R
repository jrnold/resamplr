context("jackknife")

expect_jackknife <- function(x) {
  expect_is(x, "data.frame")
  expect_named(x, c("jackknife", ".id"))
  expect_is(x$jackknife, "list")
  expect_true(all(map_lgl(x$jackknife, is.resample)))
  expect_is(x$.id, "character")
}

local({
  dat <- tibble(a = c(1, 1, 2, 3))

  test_that("jackknife.data.frame works as expected", {
    x <- jackknife(dat)
    expect_jackknife(x)
    expect_identical(nrow(x), nrow(dat))
    expect_identical(
      x$jackknife,
      map(list(2:4, c(1L, 3:4), c(1:2, 4L), c(1:3)), resample, data = dat)
    )
  })

  test_that("jackknife.grouped_df works as expected", {
    gdat <- group_by(dat, a)
    x <- jackknife(gdat)
    expect_jackknife(x)
    expect_identical(nrow(x), n_groups(gdat))
    expect_identical(
      x$jackknife,
      map(list(3:4, c(1:2, 4L), 1:3), resample, data = gdat))
  })

})
