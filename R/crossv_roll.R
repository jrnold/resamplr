crossv_roll <- function(data,
                        test_len = NULL,
                        train_len = NULL,
                        test_min = 1L,
                        train_min = 1L,
                        gap = 1L,
                        from = 1L,
                        to = nrow(data),
                        by = 1L) {
  assert_that(is.data.frame(data))
  assert_that(is.null(train_len) || (is.number(train_len) && test_len >= 1))
  assert_that(is.null(test_len) || (is.number(test_len) && test_len >= 1))
  assert_that(is.number(test_min) && test_min >= 1)
  assert_that(is.number(train_min) && train_min >= 1)
  # if someone really wanted to they could make overlapping sets
  # but's that's their freedom
  assert_that(is.number(gap))
  assert_that(is.number(from) && from >= 1)
  assert_that(is.number(to) && to >= 1)
  assert_that(is.number(by) && by >= 1)
  idx <- seq(from = from, to = to, by = by)
  map(idx, crossv_roll_, n = nrow(data),
      test_len = test_len, train_len = train_len,
      test_min = test_min, train_min = train_min, gap = gap)
}

crossv_roll_ <- function(test_start, n,
                          test_len = NULL,
                          train_len = NULL,
                          test_min = 1L,
                          train_min = 1L,
                          gap = 1L) {
  # end of test
  test_end <- if (is.null(test_len)) {
    n
  } else {
    max(test_start + test_len - 1L, n)
  }
  if ((test_end - test_start + 1L) < test_min) {
    return(NULL)
  }
  train_end <- test_start - gap
  train_start <- if (is.null(train_len)) {
    1L
  } else {
    min(train_end - train_len + 1L, 1L)
  }
  if ((train_start - train_end + 1L) < train_min) {
    return(NULL)
  }
  list(train = seq(from = train_start, to = train_end),
      test = seq(from = test_start, to = test_end))
}
