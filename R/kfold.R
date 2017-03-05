#' Generate cross-validated k-fold test-training pairs
#'
#' @param data A data frame
#' @param k The number of folds
#' @param shuffle If \code{TRUE}, randomly assign observations to folds.
#'   Otherwise, observations are sequentially assigned to folds.
#' @param stratify If \code{TRUE}, within each group observations are split
#'   into folds, and those folds combined. If \code{FALSE}, groups are assigned
#'   into folds.
#' @param ... Passed to methods
#' @return A data frame with \code{k} rows and the following columns:
#' \describe{
#' \item{train}{A list of \code{\link{resample}} objects. Training sets.}
#' \item{test}{A list of \code{\link{resample}} objects. Test sets.}
#' \item{.id}{Character vector. ID of the fold.}
#' }
#' @export
crossv_kfold <- function(data, k, ...) {
  UseMethod("crossv_kfold")
}

#' @rdname crossv_kfold
#' @export
crossv_kfold.data.frame <- function(data, k = 5, shuffle = TRUE, ...) {
  df <- as_tibble(transpose(split_kfold(seq_len(nrow(data)), shuffle = shuffle)))
  df[[".id"]] <- id(k)
  df
}

#' @rdname crossv_kfold
#' @export
crossv_kfold.grouped_df <- function(data, k = 5, shuffle = TRUE,
                                    stratify = FALSE, ...) {
  idx <- group_indices_lst(data)
  if (stratify) {
    g <- transpose(map(idx, split_kfold, k = k, shuffle = shuffle))
    test_train(data, test = flatten_int(map(g, "test")),
               train = flatten_int(map(g, "train")))
  } else {
    g <- split_kfold(seq_along(idx), k = k, shuffle = shuffle)
    test_train(data,
               test = flatten_int(idx[g$test]),
               train = flatten_int(idx[g$train]))
  }
}

split_n <- function(idx, k, shuffle = TRUE) {
  if (shuffle) {
    n <- length(idx)
    folds <- sample(rep(seq_len(k), length.out = n), n, replace = FALSE)
  } else {
    folds <- cut(idx, k, include.lowest = TRUE, labels = FALSE)
  }
  split(idx, folds)
}

split_kfold <- function(idx, k, shuffle = shuffle) {
  f <- function(i) list(train = setdiff(i, idx), test = i)
  map(split_n(idx, k, shuffle = shuffle), f)
}
