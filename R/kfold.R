#' Generate cross-validated k-fold test-training pairs
#'
#' Generate cross-validated k-fold test-training pairs.
#'
#' @param x A data frame or vector
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
#' @example inst/examples/ex-crossv_kfold.R
crossv_kfold <- function(x, k, ...) {
  UseMethod("crossv_kfold")
}

#' @rdname crossv_kfold
#' @export
crossv_kfold.data.frame <- function(x, k = 5L, shuffle = TRUE, ...) {
  to_crossv_df(crossv_kfold_(nrow(x), k = k, shuffle = shuffle), x)
}

#' @rdname crossv_kfold
#' @importFrom dplyr summarise_ group_by_ mutate_
#' @importFrom purrr map_df
#' @export
crossv_kfold.grouped_df <- function(x, k = 5L, shuffle = TRUE,
                                    stratify = FALSE, ...) {
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(x)
  if (stratify) {
    f <- function(g) {
      crossv_kfold_(length(g), k = k, shuffle = shuffle)
    }
    res <- summarise_(group_by_(map_df(idx, f), ".id"),
                      train = ~ list(flatten_int(idx[flatten_int(train)])),
                      test = ~ list(flatten_int(idx[flatten_int(test)])))
  } else {
    res <- mutate_(crossv_kfold_(length(idx), k, shuffle = shuffle),
                   train = ~ map(train, function(i) flatten_int(idx[i])),
                   test = ~ map(test, function(i) flatten_int(idx[i])))
    res
  }
  to_crossv_df(res, x)[ , c("train", "test", ".id")]
}

crossv_kfold_ <- function(n, k = 5L, shuffle = TRUE, ...) {
  x <- seq_len(n)
  f <- function(i) {
    tibble(train = list(setdiff(x, i)), test = list(i), .id = i)
  }
  purrr::map_df(partition(x, as.integer(k), shuffle = shuffle), f)
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
