#' Generate Jackknife Subsamples
#'
#' Generate jackknife subsamples. For data with \eqn{n} elements,
#' the jackknife generates \eqn{n} subsamples, each with one element
#' deleted. The generalized (delete-\eqn{p}, block) jackknife generates
#' subsamples by deleting \code{p} observations.
#'
#' @param data A data frame or vector
#' @param p The number of elements to delete. \code{p = 1} is the ordinary
#'    (delete-one) jackknife. \code{p > 1} is the block (delete-\eqn{p})
#'    jackknife.
#' @param ... Arguments passed to methods
#' @templateVar numrows \code{n} rows, for \code{p = 1}, or more generally, \eqn{{n}\choose{p}}{n choose p} rows, and
#' @template return_resample_df
#' @references
#' \itemize{
#' \item{Davison, A. C. & Hinkley, D. V. (1997) Bootstrap Methods and Their Applications. Cambridge University Press, Cambridge. ISBN 0-521-57391-2}
#' \item{Tukey, J. W. (1958). "Bias and confidence in not quite large samples". The Annals of Mathematical Statistics.  \href{https://dx.doi.org/10.1214/aoms/1177706647}{doi:10.1214/aoms/1177706647}.}
#' \item{Efron, B.; Stein, C. (May 1981). "The Jackknife Estimate of Variance". The Annals of Statistics. \href{https://dx.doi.org/10.1214/aos/1176345462}{doi:10.1214/aos/1176345462}.}
#' }
#' @export
jackknife <- function(data, p, ...) {
  UseMethod("jackknife")
}

# Note: this code is largely redundant and the same as crossv_loo
# It could be replaced by calling crossv_lpo, and then manipulating the returned
# data frame.

#' @describeIn jackknife Delete rows from a data frame.
#' @export
jackknife.data.frame <- function(data, p = 1L, ...) {
  to_resample_df(jackknife_(nrow(data), p = p), data)
}

#' @describeIn jackknife Delete groups from a grouped data frame.
#' @export
jackknife.grouped_df <- function(data, p = 1L, ...) {
  idx <- group_indices_lst(data)
  res <- mutate_(jackknife_(length(idx), p = p),
                 sample = ~ map(sample, function(i) flatten_int(idx[i])))
  to_resample_df(res, data)
}

jackknife_ <- function(n, p = 1L, ...) {
  assert_that(is.number(n) && n >= 1)
  assert_that(is.number(p) && p >= 1)
  idx <- seq_len(n)
  f <- function(i) setdiff(idx, i)
  if (p == 1) {
    res <- map(idx, f)
  } else {
    res <- utils::combn(idx, p, FUN = f, simplify = FALSE)
  }
  tibble(sample = res, .id = seq_along(res))
}
