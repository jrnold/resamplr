#' Generate cross-validated leave-p-out test-training pairs
#'
#' Generate cross-validated leave-p-out (including leave-one-out)
#' test-training pairs.
#'
#' @param x A data frame or vector
#' @param p The number of elements to include in the test set.
#' @param ... Passed to methods
#' @return A data frame with \eqn{\binom{n}{p}}{n choose p} rows and the
#' following columns:
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
  to_crossv_df(crossv_lpo_(nrow(x), p = p), x)
}

#' @rdname crossv_lpo
#' @export
crossv_lpo.grouped_df <- function(x, p = 1L, ...) {
  idx <- group_indices_lst(x)
  res <- mutate_(crossv_lpo_(length(idx), p = p),
                 train = ~ map(train, function(i) flatten_int(idx[i])),
                 test = ~ map(test, function(i) flatten_int(idx[i])))
  to_crossv_df(res, x)
}

crossv_lpo_ <- function(n, p = 1L, ...) {
  assert_that(is.number(n) && n >= 1)
  assert_that(is.number(p) && p >= 1)
  p <- as.integer(p)
  x <- seq_len(n)
  f <- function(i) {
    tibble(train = list(setdiff(x, i)), test = list(i), .id = i)
  }
  if (p == 1) {
    map_df(x, f)
  } else {
    bind_rows(utils::combn(x, p, FUN = f, simplify = FALSE))
  }
}

#' @export
#' @rdname crossv_lpo
crossv_loo <- function(x) crossv_lpo(x, p = 1L)
