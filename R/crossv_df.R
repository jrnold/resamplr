#' Generate cross-validation test-training sets manually
#'
#' Generate cross-validation test-training sets from manually specified
#' indices of observations in the test and and training sets.
#'
#' @param data A data frame
#' @param train A list of integer vectors or an integer vector, with the indices of the training set.
#'   If \code{train} is \code{NULL}, then the training set is the complement of
#'   the test set.
#' @param test A list of integer vectors or an integer vector, with the indices of the test set.
#'   If \code{test} is \code{NULL}, then the test set is the complement of
#'   the training set.
#' @param ... Arguments passed to methods
#' @example inst/examples/ex-crossv_df.R
#' @export
crossv_df <- function(data, ...) {
  UseMethod("crossv_df")
}

#' @describeIn crossv_df Partition a data frame into test and training sets by row.
#' @export
crossv_df.data.frame <- function(data, train = NULL, test = NULL, ...) {
  assert_that(!is.null(train) || !is.null(test))
  if (is.integer(test)) test <- list(test)
  assert_that(is.null(test) ||
              (is.list(test) && all(map_lgl(test, is.integer))))
  if (is.integer(train)) test <- list(train)
  assert_that(is.null(train) ||
                (is.list(train) && all(map_lgl(train, is.integer))))
  if (is.list(test) && is.list(train)) {
    assert_that(length(train) == length(test))
  }
  to_crossv_df(crossv_df_(nrow(data), train, test), data)
}

#' @describeIn crossv_df Partition a grouped data frame into test and training
#'    sets by group.
#' @export
crossv_df.grouped_df <- function(data, train = NULL, test = NULL, ...) {
  assert_that(!is.null(train) || !is.null(test))
  if (is.integer(test)) test <- list(test)
  assert_that(is.null(test) ||
                (is.list(test) && all(map_lgl(test, is.integer))))
  if (is.integer(train)) test <- list(train)
  assert_that(is.null(train) ||
                (is.list(train) && all(map_lgl(train, is.integer))))
  if (is.list(test) && is.list(train)) {
    assert_that(length(train) == length(test))
  }
  idx <- group_indices_lst(data)
  res <- mutate_(crossv_df_(length(idx), train, test),
                 train = ~ map(train, function(i) flatten_int(idx[i])),
                 test = ~ map(test, function(i) flatten_int(idx[i])))
  to_crossv_df(res, data)
}

crossv_df_ <- function(n, test, train) {
  test <- test %||% vector("list", length(train))
  train <- train %||% vector("list", length(test))
  f <- function(.train, .test, idx) {
    .train <- .train %||% setdiff(idx, .test)
    .test <- .test %||% setdiff(idx, .train)
    tibble(train = list(.train), test = list(.test))
  }
  res <- map2_df(test, train, f, idx = seq_len(n))
  res[[".id"]] <- seq_len(nrow(res))
  res
}
