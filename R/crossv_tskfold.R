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
#' @references
#' \itemize{
#' \item{Scikit-learn v. 0.18.1. Cross-validation: evaluating estimator performance.
#'    \href{http://scikit-learn.org/stable/modules/cross_validation.html#cross-validation}{URL}.}
#' }
#' @export
crossv_tskfold <- function(data, K, ...) {
  UseMethod("crossv_tskfold")
}

#' @export
crossv_tskfold.data.frame <- function(data,
                                      K = 5L,
                                      train_min = 1L,
                                      train_max = K - 1L,
                                      test_min = 1L,
                                      test_max = 1L, ...) {
  out <- crossv_tskfold_(resample_idx_len(data), K,
                         train_min = train_min, train_max = train_max,
                         test_min = test_min, test_max = test_max)
  for (i in c("train", "test")) {
    out[[i]] <- resample_lst(data, out[[i]], ...)
  }
  out
}

#' @export
crossv_tskfold.grouped_df <- function(data, K = 5L, ...) {
  stop("crossv_tskfold.grouped_df not implemented yet")
}

crossv_tskfold_1fold <- function(i, folds, test_min, test_max,
                                 train_min, train_max) {
  K <- length(folds)
  train_folds <- seq(max(i - 1L, train_min), min(K, train_max), by = 1L)
  test_folds <- seq(max(i, test_min), min(K, test_max), by = 1L)
  if (length(train_folds) && length(test_folds)) {
    tibble(train = list(as_vector(folds[train_folds])),
           test = list(as_vector(folds[test_folds])))
  } else {
    NULL
  }
}

#' @importFrom purrr compact
crossv_tskfold_ <- function(n, K = 5L,
                            train_min = 1L, train_max = KL,
                            test_min = 1L, test_max = 1L) {
  folds <- partition(seq_len(n), as.integer(K), shuffle = FALSE)
  map_df(seq_len(n), folds = folds,
         train_min = train_min, train_max = train_max,
         test_min = test_min, test_max = test_max)
}
