context("crossv_kfold")

local({
  dat <- tibble(a = rep(1:3, 2))
  dat_grouped <- group_by(dat, a)

  test_that("crossv_lpo.data.frame works as expected", {
    p <- 2
    n <- nrow(dat)
    x <- crossv_lpo(dat, p = p)
    expect_crossv_df(x)
    expect_equal(nrow(x), choose(n, p))
    expect_true(all(map_int(x$train, ~ dim(.x)[1]) == n - p))
    expect_true(all(map_int(x$test, ~ dim(.x)[1]) == p))
  })


  test_that("crossv_lpo.grouped_df works as expected", {
    p <- 2
    G <- n_groups(dat_grouped)
    x <- crossv_lpo(dat_grouped, p)
    expect_crossv_df(x)
    expect_equal(nrow(x), choose(G, p))
  })


  test_that("crossv_lpo.default works as expected", {
    p <- 2
    v <- 1:10
    n <- length(v)
    x <- crossv_lpo(v, p)
    expect_crossv_df(x, data = FALSE)
    expect_equal(nrow(x), choose(n, p))
    expect_true(all(map_int(x$train, length) == (n - p)))
    expect_true(all(map_int(x$test, length) == p))
  })

  test_that("crossv_loo works as expected", {
    p <- 1L
    n <- nrow(dat)
    x <- crossv_loo(dat)
    expect_crossv_df(x)
    expect_equal(nrow(x), n)
    expect_true(all(map_int(x$train, ~ dim(.x)[1]) == (n - 1)))
    expect_true(all(map_int(x$test, ~ dim(.x)[1]) == 1))
  })

})

