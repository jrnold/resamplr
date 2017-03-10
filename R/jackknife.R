#' Generate Jackknife Subsamples
#'
#' Generate jackknife subsamples.
#'
#' @param x A data frame or vector
#' @param p The number of elements to delete; \code{p = 1} is the standard
#'    jackknife, while \code{p > 1} is the delete-p jackknife.
#' @param ... Passed to methods
#' @return A data frame with \eqn{\binom{n}{p}}{n choose p} rows and the
#' following columns:
#' \describe{
#' \item{sample}{A list of \code{\link{resample}} objects.}
#' \item{.id}{Character vector. ID of the sample.}
#' }
#' @export
jackknife <- function(x, p, ...) {
  UseMethod("jackknife")
}

# Note: this code is largely redundant and the same as crossv_loo
# It could be replaced by calling crossv_lpo, and then manipulating the returned
# data frame.

#' @rdname jackknife
#' @export
jackknife.data.frame <- function(x, p = 1L, ...) {
  idx <- seq_len(nrow(x))
  to_resample_df(jackknife(idx, p = p), x)
}

#' @rdname jackknife
#' @export
jackknife.grouped_df <- function(x, p = 1L, ...) {
  idx <- group_indices_lst(x)
  res <- mutate_(jackknife(idx, p = p),
                 sample = ~ map(sample, flatten_int))
  to_resample_df(res, x)
}

#' @export
#' @importFrom dplyr bind_rows
#' @rdname jackknife
jackknife.default <- function(x, p = 1L, ...) {
  assert_that(is.number(p) && p >= 1)
  f <- function(i) {
    tibble(sample = list(setdiff(x, i)))
  }
  if (p == 1) {
    res <- map_df(x, f)
  } else {
    res <- bind_rows(utils::combn(x, p, FUN = f, simplify = FALSE))
  }
  res[[".id"]] <- seq_len(nrow(res))
  res
}

to_resample_df <- function(x, .data) {
  x[["sample"]] <- resample_lst(x[["sample"]], .data)
  x
}