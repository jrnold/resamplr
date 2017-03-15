local({
  context("holdout_n")

  dat <- tibble(a = c(rep("a", 5), rep("b", 3), rep("c", 2)))
  dat_grouped <- group_by(dat, a)

  test_that("holdout_n.data.frame works as expected", {
    size <- 2
    K <- 5
    x <- holdout_n(dat, size = size, K = K)
    expect_crossv_df(x)
    expect_equal(nrow(x), K)
    expect_true(all(map_int(x$test, ~ dim(.x)[1]) == size))
    expect_true(all(map_int(x$train, ~ dim(.x)[1]) == (nrow(dat) - size)))
  })

  test_that("holdout_n.grouped_df works as expected", {
    size <- 2
    K <- 5
    x <- holdout_n(dat_grouped, size = size, K = K, stratify = FALSE)
    expect_crossv_df(x)
    expect_equal(nrow(x), K)
  })

  test_that("holdout_n.grouped_df stratify = TRUE works as expected", {
    size <- 1
    K <- 5
    x <- holdout_n(dat_grouped, size = size, K = K, stratify = TRUE)
    expect_crossv_df(x)
    expect_equal(nrow(x), K)
  })

  test_that("holdout_n.data.frame shuffle = FALSE works as expected", {
    size <- 2
    K <- 1
    x <- holdout_n(dat, size = size, K = K, shuffle = FALSE)
    expect_crossv_df(x)
    expect_equal(nrow(x), K)
    expect_identical(x$train[[1]], resample(dat, 3:10))
    expect_identical(x$test[[1]], resample(dat, 1:2))
  })

  context("holdout_frac")

  test_that("holdout_frac.data.frame works as expected", {
    size <- 0.3
    n <- nrow(dat)
    heldout <- round(size * n)
    K <- 2
    x <- holdout_frac(dat, size = size, K = K)
    expect_crossv_df(x)
    expect_equal(nrow(x), K)
    expect_true(all(map_int(x$test, ~ dim(.x)[1]) == heldout))
    expect_true(all(map_int(x$train, ~ dim(.x)[1]) == (n - heldout)))
  })

  test_that("holdout_frac.grouped_df works as expected", {
    size <- 0.3
    K <- 2
    x <- holdout_frac(dat_grouped, size = size, K = K, stratify = FALSE)
    expect_crossv_df(x)
    expect_equal(nrow(x), K)
  })

  test_that("holdout_frac.grouped_df stratify = TRUE works as expected", {
    size <- 0.3
    K <- 2
    x <- holdout_frac(dat_grouped, size = size, K = K, stratify = TRUE)
    expect_crossv_df(x)
    expect_equal(nrow(x), K)
  })

  test_that("holdout_n.grouped_df stratify = TRUE, prob works as expected", {
    size <- 2
    K <- 2
    prob <- runif(nrow(dat_grouped))
    x <- holdout_n(dat_grouped, size = size, K = K, stratify = TRUE,
                   prob = prob)
    expect_crossv_df(x)
    expect_equal(nrow(x), K)
  })

})
