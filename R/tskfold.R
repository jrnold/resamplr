#' Generate cross-validated time-series k-fold test-training pairs
#'
#' Splits data into test/train sets for cross-validating time series
#' data. In each split, the indices are increasing, so only previous data
#' are used to test future data.
#'
#' The data are split into \eqn{k \geq 2} folds, where for each
#' \eqn{i \in 2:k}{i in 2:k}, the \eqn{i}-th fold is the test set
#' and the \eqn{1:(i - 1)}-th folds are the training set.
#'
#' @param x A data frame or vector
#' @param k The number of folds
#' @param ... Passed to methods
#' @return A data frame with \code{k} rows and the following columns:
#' \describe{
#' \item{train}{A list of \code{\link{resample}} objects. Training sets.}
#' \item{test}{A list of \code{\link{resample}} objects. Test sets.}
#' \item{.id}{Character vector. ID of the fold.}
#' }
#' @export
#' @example inst/examples/ex-crossv_tskfold.R
crossv_tskfold <- function(x, k, ...) {
  UseMethod("crossv_tskfold")
}

#' @rdname crossv_tskfold Rows (observations) are split into folds.
#'    The ordering of the data frame is assmed to be meaningful.
#' @export
crossv_tskfold.data.frame <- function(x, k = 5L, ...) {
  to_crossv_df(crossv_tskfold_(nrow(x), k), x)
}

#' @describeIn crossv_tskfold The groups are split into folds. The
#'   ordering of the groups is assumed to be meaningful. No stratification
#'   option is provided since the cases in which stratification can usually
#'   be rewritten to operate on the groups rather than within groups.
#' @importFrom dplyr summarise_ group_by_ mutate_
#' @importFrom purrr map_df
#' @export
crossv_tskfold.grouped_df <- function(x, k = 5L, ...) {
  idx <- group_indices_lst(x)
  res <- mutate_(crossv_tskfold_(length(idx), k),
                 train = ~ map(train, function(i) flatten_int(idx[i])),
                 test = ~ map(test, function(i) flatten_int(idx[i])))
  to_crossv_df(res, x)
}

# work with index integers
crossv_tskfold_ <- function(n, k = 5L) {
  folds <- partition(seq_len(n), as.integer(k), shuffle = FALSE)
  f <- function(i) {
    tibble(train = list(folds[seq_len(i - 1L)]), test = list(folds[i]))
  }
  # test that k >= 2 earlier
  purrr::map_df(2:k, f)
}
