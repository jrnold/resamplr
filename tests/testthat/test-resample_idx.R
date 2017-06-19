context("resample_idx")

test_that("resample_idx.data.frame works as expected", {
  dat <- tibble(a = 1:6)
  samples <- list(1:2, 2:6)
  expect_identical(resample_idx(dat, samples),
                   tibble(sample = resample_lst(dat, samples),
                          .id = seq_along(samples)))

})

test_that("resample_idx.grouped_df works as expected", {
  dat <- group_by(tibble(a = rep(1:3, 2)), a)
  samples <- list(1L, 2:3)
  expect_identical(resample_idx(dat, samples),
                   tibble(sample =
                            resample_lst(dat, list(c(1L, 4L),
                                                   c(2L, 5L, 3L, 6L))),
                          .id = seq_along(samples)))

})
