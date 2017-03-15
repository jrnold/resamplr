context("crossv_kfold")

local({
  dat <- tibble(a = c(rep("a", 5), rep("b", 3), rep("c", 2)))
  dat_grouped <- group_by(dat, a)

  test_that("crossv_kfold.data.frame works as expected", {
    K <- 5
    n <- nrow(dat)
    x <- crossv_kfold(dat, K = K)
    expect_crossv_df(x)
    expect_equal(nrow(x), K)
    expect_true(all(map_int(x$train, ~ dim(.x)[1]) == 4 * (n / K)))
    expect_true(all(map_int(x$test, ~ dim(.x)[1]) == n / K))
  })

  test_that("crossv_kfold.data.frame works as expected", {
    K <- 3
    n <- n_groups(dat)
    x <- crossv_kfold(dat_grouped, K = K)
    expect_crossv_df(x)
    expect_equal(nrow(x), K)
  })

  test_that("crossv_kfold.grouped_df stratify = TRUE works as expected", {
    K <- 2
    x <- crossv_kfold(dat_grouped, K = K, stratify = TRUE)
    expect_crossv_df(x)
    expect_equal(nrow(x), K)
  })

  test_that("crossv_kfold works when not shuffled", {
    K <- 3
    x <- crossv_kfold(dat, K = K, shuffle = FALSE)
    expect_crossv_df(x)
    expect_equal(nrow(x), K)
  })

})
