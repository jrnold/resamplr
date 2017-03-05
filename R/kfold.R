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
  assert_that(is.number(k) && k >= 0)
  assert_that(is.flag(shuffle))
  folds <- split_kfold(seq_len(nrow(data)), k, shuffle = shuffle)
  tibble(
    train = resample_lst(data, unname(map(folds, "train"))),
    test = resample_lst(data, unname(map(folds, "test"))),
    .id = id(k)
  )
}

#' @rdname crossv_kfold
#' @export
crossv_kfold.grouped_df <- function(data, k = 5, shuffle = TRUE,
                                    stratify = FALSE, ...) {
  assert_that(is.number(k) && k >= 0)
  assert_that(is.flag(shuffle))
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(data)
  if (stratify) {
    f <- function(j) {
      .f <- function(i) flatten_int(map(i, j))
      unname(resample_lst(data, map(folds, .f)))
    }
    folds <- transpose(map(idx, split_kfold, k = k, shuffle = shuffle))
    tibble(
      train = f("train"),
      test = f("test"),
      .id = id(k)
    )
  } else {
    g <- transpose(split_kfold(seq_along(idx), k = k, shuffle = shuffle))
    tibble(
      train = resample_lst(data,
                           unname(map(g[["train"]],
                                      function(i) flatten_int(idx[i])))),
      test = resample_lst(data,
                          unname(map(g[["test"]],
                                     function(i) flatten_int(idx[i])))),
      .id = id(k)
    )
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
  f <- function(i) list(train = setdiff(idx, i), test = i)
  map(split_n(idx, k, shuffle = shuffle), f)
}
