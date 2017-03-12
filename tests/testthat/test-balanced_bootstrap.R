context("balanced_bootstrap")

local({
  dat <- tibble(a = rep(1:3, 4))
  gdat <- group_by(dat, a)

  test_that("bootstrap works as expected", {
    k <- 3
    x <- balanced_bootstrap(dat, k)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
    # check all rows appear k times
    expect_identical(sort(flatten_int(map(x$sample, as.integer))),
                     rep(seq_len(nrow(dat)), each = k))
  })

  test_that("balanced_bootstrap.grouped_df(stratify = FALSE) works as expected", {
    k <- 3
    x <- balanced_bootstrap(gdat, k, stratify = FALSE)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
    # check all rows appear k times
    expect_identical(sort(flatten_int(map(x$sample, as.integer))),
                     rep(seq_len(nrow(dat)), each = k))
  })

  test_that("balanced_bootstrap.grouped_df(stratify = TRUE)", {
    k <- 3
    x <- balanced_bootstrap(gdat, k, stratify = TRUE)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
    # check all rows appear k times
    expect_identical(sort(flatten_int(map(x$sample, as.integer))),
                     rep(seq_len(nrow(dat)), each = k))
  })

})
