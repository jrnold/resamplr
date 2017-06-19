#' Generate cross-validation test-training sets manuall
#'
#' Generate cross-validation test-training sets from manually specified
#' indices of observations in the test and and training sets.
#'
#' @param data A data frame
#' @param train A list of integer vectors or an integer vector, with the indices of the training set.
#'   If \code{train} is \code{NULL}, then the training set is the complement
#'   the test set.
#' @param test A list of integer vectors or an integer vector, with the indices of the test set.
#'   If \code{test} is \code{NULL}, then the test set is the complement of
#'   the training set.
#' @param ... Arguments passed to methods
#' @example inst/examples/ex-holdout_idx.R
#' @template return_holdout
#' @export
holdout_idx <- function(data, train, test, ...) {
  UseMethod("holdout_idx")
}

# fill_test_train <- function(x, y, idx) {
#   x %||% map(y, function(.y) setdiff(idx, .y))
# }

#' @describeIn holdout_idx
#' @export
holdout_idx.default <- function(data, train, test, ...) {
  tibble(data = data,
         train = resample_lst(data, train),
         test = resample_lst(data, test),
         .id = seq_along(train))
}

#' @describeIn holdout_idx Partition a grouped data frame into test and training
#'    sets by group.
#' @export
holdout_idx.grouped_df <- function(data, train, test, ...) {
  idx <- group_indices_lst(data)
  f <- function(i) flatten_int(idx[i])
  holdout_idx.default(data, train = map(train, f), test = map(test, f))
}

#' @describeIn holdout_idx For \code{resmple} objects, the \code{train} and \code{test} indexes refer to index of the elements in the \code{data$idx} vector; not the values in that vector
#' @export
holdout_idx.resample <- function(data, train, test, ...) {
  # indexes have to refer to the index of elements in the `idx` element
  f <- function(i) purrr::flatten(data$idx[i])
  holdout_idx.default(data = data$data, train = map(train, f),
                      test = map(test, f))
}
