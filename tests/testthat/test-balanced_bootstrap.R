context("balanced_bootstrap")

local({
  dat <- tibble(a = rep(1:3, 4))
  gdat <- group_by(dat, a)

  test_that("bootstrap works as expected", {
    R <- 3
    x <- balanced_bootstrap(dat, R)
    expect_resample_df(x)
    expect_equal(nrow(x), R)
    # check all rows appear R times
    expect_identical(sort(flatten_int(map(x$sample, as.integer))),
                     rep(seq_len(nrow(dat)), each = R))
  })

  test_that(paste0("balanced_bootstrap.grouped_df(stratify = FALSE)",
                   " works as expected"), {
    R <- 3
    x <- balanced_bootstrap(gdat, R, stratify = FALSE)
    expect_resample_df(x)
    expect_equal(nrow(x), R)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
    # check all rows appear R times
    expect_identical(sort(flatten_int(map(x$sample, as.integer))),
                     rep(seq_len(nrow(dat)), each = R))
  })

  test_that("balanced_bootstrap.grouped_df(stratify = TRUE)", {
    R <- 3
    x <- balanced_bootstrap(gdat, R, stratify = TRUE)
    expect_resample_df(x)
    expect_equal(nrow(x), R)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
    # check all rows appear R times
    expect_identical(sort(flatten_int(map(x$sample, as.integer))),
                     rep(seq_len(nrow(dat)), each = R))
  })

})
