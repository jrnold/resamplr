#' Generate test/train resample objects
#'
#' Generate test and train resample objects.
#' The function \code{resample_holdout} splits the set into test and training
#' sets with specified fractions in each, while \code{resample_holdout_n} splits
#' them using specified numbers of observations.
#'
#' @param test Fraction of observations (or groups) in the test set.
#'   If \code{NULL}, then it defaults to \code{1 - train}.
#' @param train Fraction of observations (or groups) in the training set.
#'   If \code{NULL}, then it defaults to \code{1 - test}.
#'   and training samples.
#' @param data A data table
#' @param shuffle If \code{TRUE}, the observations are randomly assigned to the
#'   test and training sets. If \code{FALSE}, then the first \code{train} of
#'   the observations are assigned to the training set, and the next
#'   \code{test} of the observations are assigned to the test set.
#'
#' @param stratify If \code{TRUE}, then test-train splits are done within each
#'   code group, so that the final test and train subsets have approximately
#'   equal proportions of groups. If \code{FALSE}, the the test-train splits
#'   splits groups into the testing and training sets.
#' @param ... Arguments passed to methods
#' @return A named list of two \code{\link[modelr]{resample}} objects
#'   for the "test" and "train" sets.
#' @export
resample_holdout <- function(data, ...) {
  UseMethod("resample_holdout")
}

#' @rdname resample_holdout
#' @export
resample_holdout.data.frame <- function(data, test = 0.3, train = 0.7,
                                        shuffle = TRUE, ...) {
  n <- nrow(data)
  if (!is.null(test) && !is.null(train) && (test + train) < 0 &&
      (test + train) > 1) {
    stop("`test` and `train` must sum to a fraction between 0 and 1.",
         call. = FALSE)
  }
  test <- if (!is.null(test)) round(test * n)
  train <- if (!is.null(train)) round(train * n)
  resample_holdout_n(data, test = test, train = train, shuffle = shuffle)
}

#' @rdname resample_holdout
#' @export
resample_holdout.grouped_df <- function(data, test = 0.3, train = 0.7,
                                        shuffle = TRUE, stratify = FALSE, ...) {
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

#' @rdname resample_holdout
#' @export
resample_holdout_n <- function(data, ...) {
  UseMethod("resample_holdout_n")
}

#' @rdname resample_holdout
#' @export
resample_holdout_n.data.frame <- function(data, test = 1, train = NULL,
                                          shuffle = TRUE, ...) {
  purrr::invoke(test_train,
                split_test_train_n(seq_len(data), test = test,
                                   train = train, shuffle = shuffle),
                data = data)
}

#' @rdname resample_holdout
#' @export
resample_holdout_n.grouped_df <- function(data, test = 1, train = NULL,
                                        shuffle = TRUE, stratify = FALSE, ...) {
  idx <- group_indices_lst(data)
  if (stratify) {
    idx <- map(idx, split_test_train_n, test = test, train = train,
               shuffle = shuffle)
    test_train(data, test = flatten_int(map(idx, "test")),
               train = flatten_int(map(idx, "train")))
  } else {
    g <- split_test_train_n(seq_along(idx), test, train, shuffle = shuffle)
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


