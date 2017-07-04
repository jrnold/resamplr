#' Generate cross-validated leave-p-out test/training pairs
#'
#' Generate cross-validated leave-one-out or leave-p-out
#' test/training pairs.
#'

#' The function \code{crossv_loo} generates leave-one-out test/training pairs.
#' The function \code{cross_lpo} generates the more general leave-p-out cross-validation test/training pairs.
#'
#' @template param-expr
#' @template param-n
#' @param p The number of elements to include in the test set. For example, specify \code{p = 1} for leave-one-out cross validation.
#' @param ... Arguments passed to \code{\link{crossv_lpo_n}}.
#' @template param-extractor
#' @family cross-validation functions
#' @inherit crossv_kfold references
#' @templateVar numrows \code{combn(n, p)}
#' @templateVar f \code{crossv_lpo} and \code{crossv_loo}
#' @templateVar fn \code{crossv_lpo_n} and \code{crossv_loo_n}
#' @template return_crossv_df
#' @export
crossv_lpo <- function(expr, p = 1L, n = NULL, extractor = NULL) {
  expr <- enquo(expr)
  n <- n %||% idx_len(expr)
  out <- crossv_lpo_n(n, p = p)
  for (i in c("test", "train")) {
    out[[i]] <- lazy_sample_lst(UQ(expr), out[[i]], extractor = extractor)
  }
  out
}

#' @rdname crossv_lpo
#' @export
#' @importFrom purrr map_df
#' @importFrom dplyr bind_rows
#' @importFrom utils combn
crossv_lpo_n <- function(n, p = 1L) {
  # Note: jackknife_n uses the same code. Update that if anything
  # changed within here.
  n <- as.integer(n)
  assert_that(is_pos_scalar_integer(n))
  p <- as.integer(p)
  assert_that(is_pos_scalar_integer(p))
  idx <- seq_len(n)
  if (p == 1) {
    test <- as.list(idx)
  } else {
    test <- combn(idx, p, simplify = FALSE)
  }
  train <- map(test, function(i) base::setdiff(idx, i))
  tibble(train = train, test = test)
}

#' @rdname crossv_lpo
#' @export
crossv_loo_n <- function(n, ...) {
  crossv_lpo_n(n, p = 1L, ...)
}

#' @rdname crossv_lpo
#' @export
crossv_loo <- function(expr, ...) {
  crossv_lpo(UQ(enquo(expr)), p = 1L, ...)
}
