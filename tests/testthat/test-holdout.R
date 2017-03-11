context("holdout_n")

local({
  dat <- tibble(a = c(rep("a", 5), rep("b", 3), rep("c", 2)))
  dat_grouped <- group_by(dat, a)

  test_that("holdout_n.data.frame works as expected", {
    n <- 2
    k <- 5
    x <- holdout_n(dat, n = n, k = k)
    expect_crossv_df(x)
    expect_equal(nrow(x), k)
    expect_true(all(map_int(x$test, ~ dim(.x)[1]) == n))
    expect_true(all(map_int(x$train, ~ dim(.x)[1]) == (nrow(dat) - n)))
  })

  test_that("holdout_n.grouped_df works as expected", {
    n <- 2
    k <- 5
    x <- holdout_n(dat_grouped, n = n, k = k, stratify = FALSE)
    expect_crossv_df(x)
    expect_equal(nrow(x), k)
  })

  test_that("holdout_n.grouped_df stratify = TRUE works as expected", {
    n <- 1
    k <- 5
    x <- holdout_n(dat_grouped, n = n, k = k, stratify = TRUE)
    expect_crossv_df(x)
    expect_equal(nrow(x), k)
  })

})
