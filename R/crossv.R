#' Generate cross-validation test-training sets manually
#'
#' Generate cross-validation test-training sets from manually specified indices of observations in the test and and training sets.
#'
#' @param data A data frame
#' @param train A list of integer vectors or an integer vector, with the indices of the training set.
#'   If \code{train} is \code{NULL}, then the training set is the complement
#'   the test set.
#' @param test A list of integer vectors or an integer vector, with the indices of the test set.
#'   If \code{test} is \code{NULL}, then the test set is the complement of
#'   the training set.
#' @param ... Arguments passed to methods
#' @export
crossv_idx <- function(data, train, test, ...) {
  tibble(train = resample_lst(data, train, ...),
         test = resample_lst(data, test, ...))
}
