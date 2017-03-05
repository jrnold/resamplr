context("crossv_kfold")
{
  dat <- tibble(a = 1:4)

  expect_crossv_kfold <- function(x, k) {
    expect_is(x, "data.frame")
    expect_named(x, c("train", "test", ".id"))
    expect_equal(nrow(x), k)
    expect_is(x$train, "list")
    expect_true(all(map_lgl(x$train, is.resample)))
    expect_is(x$test, "list")
    expect_true(all(map_lgl(x$test, is.resample)))
    expect_is(x$.id, "character")
  }

  test_that("crossv_kfold works as expected", {
    k <- 2
    set.seed(1234)
    x <- crossv_kfold(dat, k = k, shuffle = TRUE)
    expect_crossv_kfold(x, k)
    # results checked by hand
    expect_identical(unname(map(x$train, as.integer)),
                     unname(map(list(c(2, 4), c(1, 3)), as.integer)))
    expect_identical(unname(map(x$test, as.integer)),
                     unname(map(list(c(1, 3), c(2, 4)), as.integer)))
  })

  test_that("crossv_kfold shuffle = FALSE works as expected", {
    k <- 2
    set.seed(1234)
    x <- crossv_kfold(dat, k = k, shuffle = FALSE)
    expect_crossv_kfold(x, k)
    # results checked by hand
    expect_equal(map(x$train, as.integer),
                 map(list(c(3, 4), c(1, 2)), as.integer))
    expect_equal(map(x$test, as.integer),
                 map(list(c(1, 2), c(3, 4)), as.integer))
  })

  # TODO: stratified kfold

}
