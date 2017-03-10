#' Generate cross-validated leave-p-out test-training pairs
#'
#' Generate cross-validated leave-p-out (including leave-one-out)
#' test-training pairs.
#'
#' @param x A data frame or vector
#' @param p The number of elements to include in the training set
#' @param ... Passed to methods
#' @return A data frame with \code{p} rows and the following columns:
#' \describe{
#' \item{train}{A list of \code{\link{resample}} objects. Training sets.}
#' \item{test}{A list of \code{\link{resample}} objects. Test sets.}
#' \item{.id}{Character vector. ID of the fold.}
#' }
#' @export
crossv_lpo <- function(x, p, ...) {
  UseMethod("crossv_lpo")
}

#' @rdname crossv_lpo
#' @export
crossv_lpo.data.frame <- function(x, p = 1L, ...) {
  idx <- seq_len(nrow(x))
  to_crossv_df(crossv_lpo(idx, p = p), x)
}

#' @rdname crossv_lpo
#' @export
crossv_lpo.grouped_df <- function(x, p = 1L, ...) {
  idx <- group_indices_lst(x)
  res <- mutate_(crossv_lpo(idx, p = p),
                 train = ~ map(train, flatten_int),
                 test = ~ map(test, flatten_int))
  to_crossv_df(res, x)
}

#' @export
#' @importFrom dplyr bind_rows
#' @rdname crossv_lpo
crossv_lpo.default <- function(x, p = 1L, ...) {
  assert_that(is.number(p) && p >= 1)
  f <- function(i) {
    tibble(train = list(setdiff(x, i)), test = list(i))
  }
  if (p == 1) {
    res <- map_df(x, f)
  } else {
    res <- bind_rows(utils::combn(x, p, FUN = f, simplify = FALSE))
  }
  res[[".id"]] <- seq_len(nrow(res))
  res
}

#' @export
#' @rdname crossv_lpo
crossv_loo <- function(x) crossv_lpo(x, p = 1L)
