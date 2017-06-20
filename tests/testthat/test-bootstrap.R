context("bootstrap.data.frame")

expect_bootstrap <- function(x, R) {
  expect_resample_df(x)
  expect_equal(nrow(x), R)
}

local({
  n <- 4
  dat <- tibble(a = rep(1:3, n),
                w = runif(length(a)))
  gdat <- group_by(dat, a)

  test_that("bootstrap.data.frame works as expected", {
    .args <- cross_n(list(data = list(dat),
                     R = c(1L, 2L),
                     bayes = c(TRUE, FALSE),
                     weights = list(character(), "w")))
    for (.a in .args) {
      if (!length(.a$weights)) .a$weights <- NULL
      out <- invoke(bootstrap, .a)
      expect_bootstrap(out, .a$R)
    }
  })

  test_that("bootstrap.grouped_df works as expected", {
    .args <- cross_n(list(data = list(gdat),
                          R = c(1L, 2L),
                          bayes = c(TRUE, FALSE),
                          weights = list(character(), "w"),
                          groups = c(TRUE, FALSE),
                          stratify = c(TRUE, FALSE),
                          weight_groups = c(TRUE, FALSE),
                          weight_within = c(TRUE, FALSE)
    ))
    for (.a in .args) {
      if (!length(.a$weights)) .a$weights <- NULL
      if (!.a$groups) {
        expect_error(invoke(bootstrap, .a))
      } else {
        out <- invoke(bootstrap, .a)
        expect_bootstrap(out, .a$R)
      }
    }
  })



})
