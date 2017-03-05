#' Generate test/train resample objects
#'
#' Generate test and train resample objects.
#' The function \code{resample_holdout} splits the set into test and training
#' sets with specified fractions in each, while \code{resample_holdout_n} splits
#' them using specified numbers of observations.
#'
#' @param test Fraction of observations (or groups) in the test set.
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
#' @importFrom assertthat is.number
resample_holdout.data.frame <- function(data, test = 0.3, shuffle = TRUE, ...) {
  n <- nrow(data)
  assert_that(is.number(test))
  assert_that(test >= 0 & test <= 1)
  resample_holdout_n(data, test = round(n * test), shuffle = shuffle)
}

#' @rdname resample_holdout
#' @export
resample_holdout.grouped_df <- function(data, test = 0.3, shuffle = TRUE,
                                        stratify = FALSE, ...) {
  assert_that(is.number(test))
  assert_that(test >= 0 & test <= 1)
  assert_that(is.flag(shuffle))
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(data)
  if (stratify) {
    idx <- map(idx, split_test_train_frac, test = test, shuffle = shuffle)
    test_train(data, test = flatten_int(map(idx, "test")),
               train = flatten_int(map(idx, "train")))
  } else {
    g <- split_test_train_frac(seq_along(idx), test, shuffle = shuffle)
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
resample_holdout_n.data.frame <- function(data, test = 1L,
                                          shuffle = TRUE, ...) {
  assert_that(is.number(test))
  assert_that(test >= 0 & test <= nrow(data))
  assert_that(is.flag(shuffle))
  purrr::invoke(test_train,
                split_test_train_n(seq_len(nrow(data)), test = test,
                                   shuffle = shuffle),
                data = data)
}

#' @rdname resample_holdout
#' @export
resample_holdout_n.grouped_df <- function(data, test = 1L, shuffle = TRUE,
                                          stratify = FALSE, ...) {
  assert_that(is.number(test) && test >= 0)
  assert_that(is.flag(shuffle))
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(data)
  if (stratify) {
    idx <- map(idx, split_test_train_n, test = test, shuffle = shuffle)
    test_train(data, test = flatten_int(map(idx, "test")),
               train = flatten_int(map(idx, "train")))
  } else {
    g <- split_test_train_n(seq_along(idx), test, shuffle = shuffle)
    test_train(data,
               test = flatten_int(idx[g$test]),
               train = flatten_int(idx[g$train]))
  }
}

test_train <- function(data, test, train) {
  list(train = resample(data, train), test = resample(data, test))
}

test_train_df <- function(data, test, train) {
  tibble(train = list(resample(data, train)),
         test = list(resample(data, test)))
}

split_test_train_n <- function(idx, test, shuffle = TRUE) {
  if (shuffle) {
    idx <- sample(idx, length(idx), replace = FALSE)
  }
  test_idx <- utils::tail(idx, test)
  list(train = setdiff(idx, test_idx), test = test_idx)
}

split_test_train_frac <- function(idx, test, shuffle = TRUE) {
  split_test_train_n(idx, test = round(test * length(idx)), shuffle = shuffle)
}
