context("roll")

test_that("roll.grouped_df works as expected", {
  dat <- tibble(a = rep(1:5))
  expect_resample_df(roll(dat, 3L, align = "left", partial = TRUE),
                     list(1:3, 2:4, 3:5, 4:5, 5L))
  expect_resample_df(roll(dat, 4L, align = "left", partial = TRUE),
                     list(1:4, 2:5, 3:5, 4:5, 5L))
  expect_resample_df(roll(dat, 3L, align = "left", partial = FALSE),
                     list(1:3, 2:4, 3:5))
  expect_resample_df(roll(dat, 3L, align = "left", partial = 2),
                     list(1:3, 2:4, 3:5, 4:5))
  expect_resample_df(roll(dat, 3L, align = "right", partial = TRUE),
                     list(1L, 1:2, 1:3, 2:4, 3:5))
  expect_resample_df(roll(dat, 3L, align = "center", partial = TRUE),
                     list(1:2, 1:3, 2:4, 3:5, 4:5))
  expect_resample_df(roll(dat, 3L, align = "center", partial = TRUE,
                          from = 2L, end = 4L, by = 2L),
                     list(1:3, 3:5))
  expect_resample_df(roll(dat, 2L, align = "left", partial = TRUE,
                          indices = as.integer(c(1, 3, 5))),
                     list(1:2, 3:4, 5L))
})

test_that("roll.grouped_df works as expected", {
  dat <- group_by(tibble(a = rep(1:5, 2)), a)
  x <- roll(dat, 3)
  expect_resample_df(x)
  expected <- list(c(1L, 6L, 2L, 7L, 3L, 8L),
       c(2L, 7L, 3L, 8L, 4L, 9L),
       c(3L, 8L, 4L, 9L, 5L, 10L),
       c(4L, 9L, 5L, 10L),
       c(5L, 10L))
  expect_identical(map(x$sample, as.integer), expected)
})
