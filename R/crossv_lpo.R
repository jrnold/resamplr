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
crossv_lpo <- function(data, p = 1L, ...) {
  data <- as_quosure(data)
  out <- crossv_lpo_n(idx_len(data), p = p, ...)
  for (i in c("test", "train")) {
    out[[i]] <- resample_lst(data, out[[i]])
  }
  out
}

#' @export
#' @importFrom purrr map_df
#' @importFrom dplyr bind_rows
#' @importFrom utils combn
crossv_lpo_n <- function(n, p = 1L, ...) {
  assert_that(is.number(n) && n >= 1)
  assert_that(is.number(p) && p >= 1)
  idx <- seq_len(n)
  if (p == 1) {
    test <- as.list(idx)
  } else {
    test <- combn(idx, p, simplify = FALSE)
  }
  train <- map(test, base::setdiff, x = idx)
  tibble(train = train, test = test)
}

#' @rdname crossv_lpo
#' @export
crossv_loo_n <- function(n, ...) {
  crossv_lpo_n(n, p = 1L, ...)
}

#' @rdname crossv_lpo
#' @export
crossv_loo <- function(data, ...) {
  crossv_lpo(as_quosure(data), p = 1L, ...)
}
