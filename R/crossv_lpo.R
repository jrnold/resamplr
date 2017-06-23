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
#' @inherit crossv_kfold references
#' @export
#' @example inst/examples/ex-crossv_lpo.R
crossv_lpo <- function(data, p, ...) {
  UseMethod("crossv_lpo")
}

#' @export
crossv_lpo.default <- function(data, p = 1L, ...) {
  out <- crossv_lpo_(resample_idx_len(data), p = p)
  for (i in c("test", "train")) {
    out[[i]] <- resample_lst(data, out[[i]], ...)
  }
  out
}

#' @describeIn crossv_lpo Generate test/train sets by leaving groups out.
#' @export
crossv_lpo.grouped_df <- function(data, p = 1L, ...) {
  out <- crossv_lpo_(resample_idx_len(data, groups = TRUE), p = p)
  out[[".test_groups"]] <- out[["test"]]
  for (i in c("test", "train")) {
    out[[i]] <- resample_lst(data, out[[i]], groups = TRUE)
  }
  out
}

#' @importFrom purrr map_df
#' @importFrom dplyr bind_rows
#' @importFrom utils combn
crossv_lpo_ <- function(n, p = 1L, ...) {
  assert_that(is.number(n) && n >= 1)
  assert_that(is.number(p) && p >= 1)
  idx <- seq_len(n)
  f <- function(i) tibble(train = setdiff(idx, i), test = i)
  if (p == 1) {
    map_df(idx, f)
  } else {
    bind_rows(combn(idx, p, FUN = f, simplify = FALSE))
  }
}

#' @rdname crossv_lpo
#' @export
crossv_loo <- function(data, ...) {
  crossv_lpo(data, p = 1L, ...)
}
