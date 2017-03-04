#' Generate test/train resample objects
#'
#' @param test,train Integer vectors with the indexes of the test
#'   and training samples.
#'   In \code{resample_holdout.data.frame}, \code{test} and \code{train}
#'   are the indexes of observations.
#'   In \code{resample_holdout.grouped_df}, \code{test} and \code{train}
#'   are group numbers.
#'   One of \code{test} or \code{train} must be non-\code{NULL}.
#' @param data A data table
#' @param ... Arguments passed to methods
#' @return A named list of two \code{\link[modelr]{resample}} objects
#'   for the "test" and "train" sets.
#' @export
resample_holdout <- function(data, ...) {
  UseMethod("resample_holdout")
}

#' @export
resample_holdout.data.frame <- function(data, test = 0.3, train = 0.7,
                                        shuffle = TRUE, ...) {
  n <- nrow(data)
  test <- if (!is.null(test)) round(test * n)
  train <- if (!is.null(train)) round(train * n)
  resample_holdout_n(data, test = test, train = train, shuffle = shuffle)
}

#' @export
resample_holdout.grouped_df <- function(data, test = 0.3, train = 0.7,
                                        shuffle = TRUE, stratify = FALSE) {
  idx <- group_indices_lst(data)
  if (stratify) {
    idx <- map(idx, split_test_train_frac, test = test, train = train,
               shuffle = shuffle)
    test_train(data, test = flatten_int(map(idx, "test")),
               train = flatten_int(map(idx, "train")))
  } else {
    g <- split_test_train_frac(seq_along(idx), test, train, shuffle = shuffle)
    test_train(data, test = flatten_int(idx[g$test]),
               train = flatten_int(idx[g$train]))
  }
}

#' @export
resample_holdout_n <- function(data, ...) {
  UseMethod("resample_holdout_n")
}

#' @export
resample_holdout_n.data.frame <- function(data, test = NULL, train = NULL,
                                          shuffle = TRUE) {
  purrr::invoke(test_train,
                split_test_train(seq_len(data), test = test, train = train,
                                 shuffle = shuffle),
                data = data)
}

#' @describeIn resample_holdout Resamples
#' @importFrom assertthat is.number
#' @export
resample_holdout_n.grouped_df <- function(data, test = NULL, train = NULL,
                                        shuffle = TRUE, stratify = FALSE) {
  idx <- group_indices_lst(data)
  if (stratify) {
    idx <- map(idx, split_test_train, test = test, train = train,
               shuffle = shuffle)
    test_train(data, test = flatten_int(map(idx, "test")),
               train = flatten_int(map(idx, "train")))
  } else {
    g <- split_test_train(seq_along(idx), test, train, shuffle = shuffle)
    test_train(data, test = flatten_int(idx[g$test]),
               train = flatten_int(idx[g$train]))
  }
}

test_train <- function(data, test, train) {
  list(test = resample(data, test), train = resample(data, train))
}

split_test_train_n <- function(idx, test = NULL, train = NULL, shuffle = TRUE) {
  n <- length(idx)
  if (is.null(test) && is.null(train)) {
    stop("Either test or train must be non-null", call. = FALSE)
  }
  # fill missing groups
  if (is.null(test)) {
    test <- length(idx) - train
  } else if (is.null(train)) {
    train <- length(idx) - test
  }
  if (shuffle) {
    idx <- sample(idx, length(idx), replace = TRUE)
  }
  list(test = idx[1:test], train = idx[test + 1:train])
}

split_test_train_frac <- function(idx, test = NULL, train = NULL,
                                  shuffle = TRUE) {
  n <- length(idx)
  if (!is.null(test)) test <- round(test * n)
  if (!is.null(train)) train <- round(train * n)
  split_test_train_n(idx, test = test, train = train, shuffle = shuffle)
}
