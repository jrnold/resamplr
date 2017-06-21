context("resample_idx")

test_that("resample_idx.default works as expected with data frame", {
  dat <- tibble(a = 1:6)
  samples <- list(1:2, 2:6)
  expect_identical(resample_idx(dat, samples),
                   tibble(sample = resample_lst(dat, samples),
                          .id = seq_along(samples)))

})

test_that("resample_idx.default works as expected with non-data frame", {
  dat <- 1:6
  samples <- list(1:2, 2:6)
  x <- resample_idx(dat, samples)
  expected <- tibble(sample = resample_lst(dat, samples),
                     .id = seq_along(samples))
  expect_identical(x, expected)
  expect_all_same_address(map(x$sample, "data"), dat)
})

test_that("resample_idx.grouped_df works as expected", {
  dat <- group_by(tibble(a = rep(1:3, 2)), a)
  samples <- list(1L, 2:3)
  x <- resample_idx(dat, samples)
  expected <- tibble(sample =
                       resample_lst(dat, list(c(1L, 4L), c(2L, 5L, 3L, 6L))),
                     .id = seq_along(samples))
  expect_identical(x, expected)
  expect_all_same_address(map(x$sample, "data"), dat)
})

test_that("resample_idx.resample works as expected", {
  dat <- tibble(alpha = letters[1:10])
  idx1 <- 2:8
  idx2 <- list(1:2, 4:5)
  x <- resample_idx(resample(dat, idx1), idx2)
  expected <- tibble(sample =
                       resample_lst(dat,
                                    list(idx1[idx2[[1]]],
                                         idx1[idx2[[2]]])),
                     .id = seq_along(samples))
  expect_identical(x, expected)
  expect_all_same_address(map(x[["sample"]], "data"), dat)
})
