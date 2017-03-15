#' Generate cross-validated leave-p-out test/training pairs
#'
#' Generate cross-validated leave-one-out or leave-p-out test/training pairs.
#' The function \code{leave-p-out} generates leave-p-out test/training pairs,
#' while the function \code{leave-one-out} convenience function for the
#' common case of leave-one-out cross-validation, \code{p = 1}.
#'
#' @param data A data frame or vector
#' @param p The number of elements to include in the test set.
#' @param ... Passed to methods
#'
#' @templateVar numrows \eqn{{n}\choose{p}}{n choose p} rows and
#' @template return_resample_df
#' @inherit crossv_kfold references
#' @export
#' @example inst/examples/ex-crossv_lpo.R
crossv_lpo <- function(data, p, ...) {
  UseMethod("crossv_lpo")
}

#' @describeIn crossv_lpo Generate test/train sets by leaving rows of the data frame out.
#' @export
crossv_lpo.data.frame <- function(data, p = 1L, ...) {
  to_crossv_df(crossv_lpo_(nrow(data), p = p), data)
}

#' @describeIn crossv_lpo Generate test/train sets by leaving groups out.
#' @export
crossv_lpo.grouped_df <- function(data, p = 1L, ...) {
  idx <- group_indices_lst(data)
  res <- mutate_(crossv_lpo_(length(idx), p = p),
                 train = ~ map(train, function(i) flatten_int(idx[i])),
                 test = ~ map(test, function(i) flatten_int(idx[i])))
  to_crossv_df(res, data)
}

crossv_lpo_ <- function(n, p = 1L, ...) {
  assert_that(is.number(n) && n >= 1)
  assert_that(is.number(p) && p >= 1)
  p <- as.integer(p)
  x <- seq_len(n)
  f <- function(i, .id) {
    tibble(train = list(setdiff(x, i)), test = list(i), .id = .id)
  }
  idx <- if (p == 1) {
    x
  } else {
    utils::combn(x, p, simplify = FALSE)
  }
  map2_df(idx, seq_along(idx), f)
}

#' @rdname crossv_lpo
#' @export
crossv_loo <- function(data) crossv_lpo(data, p = 1L)
