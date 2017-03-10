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
#' @example inst/examples/crossv_kfold.R
crossv_kfold <- function(x, k, ...) {
  UseMethod("crossv_kfold")
}

#' @rdname crossv_kfold
#' @export
crossv_kfold.data.frame <- function(x, k = 5L, shuffle = TRUE, ...) {
  assert_that(is.number(k) && k >= 1)
  assert_that(is.flag(shuffle))
  idx <- seq_len(nrow(x))
  to_crossv_df(crossv_kfold(idx, k, shuffle = shuffle), x)
}

#' @rdname crossv_kfold
#' @importFrom dplyr summarise_ group_by_ mutate_
#' @importFrom purrr map_df
#' @export
crossv_kfold.grouped_df <- function(x, k = 5L, shuffle = TRUE,
                                    stratify = FALSE, ...) {
  assert_that(is.number(k) && k >= 0)
  assert_that(is.flag(shuffle))
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(x)
  if (stratify) {
    f <- function(i) crossv_kfold(i, k = k, shuffle = shuffle)
    ret <- summarise_(group_by_(map_df(idx, f), ".id"),
                      train = ~ flatten_int(train),
                      test = ~ flatten_int(test))
  } else {
    ret <- mutate_(crossv_kfold(idx, k, shuffle = shuffle),
                   train = ~ flatten_int(train),
                   test = ~ flatten_int(test))
    ret
  }
  to_crossv_df(ret)
}

#' @export crossv_kfold
#' @rdname
crossv_kfold.default <- function(x, k = 5L, shuffle = shuffle) {
  f <- function(i) {
    tibble(train = list(setdiff(x, i)), test = list(i))
  }
  ret <- purrr::map_df(partition(x, k, shuffle = shuffle), f)
  ret[[".id"]] <- id(nrow(ret))
  ret
}

partition <- function(x, k, shuffle = TRUE) {
  if (shuffle) {
    n <- length(x)
    folds <- sample(rep(seq_len(k), length.out = n), size = n, replace = FALSE)
  } else {
    folds <- cut(x, k, include.lowest = TRUE, labels = FALSE)
  }
  split(x, folds)
}


