context("crossv_kfold")

local({
  dat <- tibble(a = c(rep("a", 5), rep("b", 3), rep("c", 2)))
  dat_grouped <- group_by(dat, a)

  test_that("crossv_kfold.data.frame works as expected", {
    k <- 5
    n <- nrow(dat)
    x <- crossv_kfold(dat, k = k)
    expect_crossv_df(x)
    expect_equal(nrow(x), k)
    expect_true(all(map_int(x$train, ~ dim(.x)[1]) == 4 * (n / k)))
    expect_true(all(map_int(x$test, ~ dim(.x)[1]) == n / k))
  })

  test_that("crossv_kfold.data.frame works as expected", {
    k <- 3
    n <- n_groups(dat)
    x <- crossv_kfold(dat_grouped, k = k)
    expect_crossv_df(x)
    expect_equal(nrow(x), k)
  })

  test_that("crossv_kfold.grouped_df stratify = TRUE works as expected", {
    k <- 2
    x <- crossv_kfold(dat_grouped, k = k, stratify = TRUE)
    expect_crossv_df(x)
    expect_equal(nrow(x), k)
  })

  test_that("crossv_kfold works when not shuffled", {
    k <- 3
    x <- crossv_kfold(dat, k = k, shuffle = FALSE)
    expect_crossv_df(x)
    expect_equal(nrow(x), k)
  })

})
