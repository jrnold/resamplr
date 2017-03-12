context("bootstrap")

local({
  dat <- tibble(a = rep(1:3, 4))
  gdat <- group_by(dat, a)

  test_that("bootstrap works as expected", {
    k <- 2
    x <- bootstrap(dat, k)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
  })

  test_that("bootstrap bayes = TRUE, works as expected", {
    k <- 2
    x <- bootstrap(dat, k, bayes = TRUE)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
  })

  test_that("bootstrap with weights argworks as expected", {
    k <- 2
    x <- bootstrap(dat, k = k, weight = "a")
    expect_resample_df(x)
    expect_equal(nrow(x), k)
  })

  test_that("bootstrap with weights arg, bayes = TRUE, works as expected", {
    k <- 2
    x <- bootstrap(dat, k = k, weight = "a", bayes = TRUE)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
  })

  test_that("bootstrap.grouped_df(stratify = FALSE) works as expected", {
    k <- 2
    x <- bootstrap(gdat, k, stratify = FALSE, groups = TRUE,
                   weight_groups = FALSE)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
  })

  test_that(paste0("bootstrap.grouped_df(stratify = TRUE, groups = FALSE)",
                   " works as expected"), {
    k <- 2
    x <- bootstrap(gdat, k, stratify = TRUE, groups = FALSE)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
  })

  test_that(paste0("bootstrap.grouped_df(stratify = FALSE, ",
                   "weight_groups = TRUE) works as expected"), {
    k <- 2
    x <- bootstrap(gdat, k, stratify = FALSE, groups = TRUE,
                   weight_groups = TRUE)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
  })

  test_that(paste0("bootstrap.grouped_df(stratify = FALSE, bayes = TRUE), ",
                   "works as expected"), {
    k <- 2
    x <- bootstrap(gdat, k, stratify = FALSE, bayes = TRUE, weights = NULL)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
  })

  test_that(paste0("bootstrap.grouped_df(stratify = FALSE, bayes = TRUE, ",
                   "weights), works as expected"), {
    k <- 2
    x <- bootstrap(gdat, k, stratify = FALSE, bayes = TRUE, weights = "a")
    expect_resample_df(x)
    expect_equal(nrow(x), k)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
  })


  test_that(paste0("bootstrap.grouped_df(stratify = TRUE) ",
                   "works as expected"), {
    k <- 2
    x <- bootstrap(gdat, k, stratify = TRUE)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
  })

  test_that(paste0("bootstrap.grouped_df(stratify = TRUE, ",
                   "weight_within = fALSE) works as expected"), {
    k <- 2
    x <- bootstrap(gdat, k, stratify = TRUE, weights = "a", groups = FALSE,
                   weight_within = FALSE)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
  })

  test_that(paste0("bootstrap.grouped_df(stratify = TRUE, bayes = TRUE), ",
                   "works as expected"), {
    k <- 2
    x <- bootstrap(gdat, k, stratify = TRUE, bayes = TRUE, weights = NULL)
    expect_resample_df(x)
    expect_equal(nrow(x), k)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
  })

  test_that(paste0("bootstrap.grouped_df(stratify = TRUE, bayes = TRUE, ",
                   "weights), works as expected"), {
    k <- 2
    x <- bootstrap(gdat, k, stratify = TRUE, bayes = TRUE, weights = "a")
    expect_resample_df(x)
    expect_equal(nrow(x), k)
    expect_is(x$.group, "list")
    expect_true(all(map_lgl(x$.group, is.integer)))
  })
})
