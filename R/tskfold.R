#' Generate cross-validated time-series K-fold test-training pairs
#'
#' Splits data into test/train sets for cross-validating time series
#' data. In each split, the indices are increasing, so only previous data
#' are used to test future data.
#'
#' The data are split into \eqn{K \geq 2} folds, where for each
#' \eqn{i \in 2:K}{i in 2:K}, the \eqn{i}-th fold is the test set
#' and folds \eqn{1, ..., (i - 1)} folds are the training set.
#'
#' @param data A data frame or vector
#' @param K The number of folds
#' @param ... Passed to methods
#' @templateVar numrows \code{K}
#' @template return_crossv_df
#'
#' @references
#' \itemize{
#' \item{Scikit-learn v. 0.18.1. Cross-validation: evaluating estimator performance.
#'    \href{http://scikit-learn.org/stable/modules/cross_validation.html#cross-validation}{URL}.}
#' }
#' @export
crossv_tskfold <- function(data, K, ...) {
  UseMethod("crossv_tskfold")
}

#' @describeIn crossv_tskfold Rows (observations) are split into folds.
#'    The ordering of the data frame is assmed to be meaningful.
#' @export
crossv_tskfold.data.frame <- function(data, K = 5L, ...) {
  assert_that(is.number(K) && K >= 2)
  to_crossv_df(crossv_tskfold_(nrow(data), K), data)
}

#' @describeIn crossv_tskfold The groups are split into folds. The
#'   ordering of the groups is assumed to be meaningful. No stratification
#'   option is provided since the cases in which stratification can usually
#'   be rewritten to operate on the groups rather than within groups.
#' @importFrom dplyr summarise_ group_by_ mutate_
#' @importFrom purrr map_df
#' @export
crossv_tskfold.grouped_df <- function(data, K = 5L, ...) {
  assert_that(is.number(K) && K >= 2)
  idx <- group_indices_lst(data)
  res <- mutate_(crossv_tskfold_(length(idx), K),
                 train = ~ map(train, function(i) flatten_int(idx[i])),
                 test = ~ map(test, function(i) flatten_int(idx[i])))
  to_crossv_df(res, data)
}

crossv_tskfold_ <- function(n, K = 5L) {
  folds <- partition(seq_len(n), as.integer(K), shuffle = FALSE)
  f <- function(i) {
    tibble(train = list(flatten_int(folds[seq_len(i - 1L)])),
           test = list(flatten_int(folds[i])),
           .id = i)
  }
  # test that K >= 2 earlier
  purrr::map_df(2:K, f)
}
