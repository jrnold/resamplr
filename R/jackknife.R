#' Generate Jackknife Subsamples
#'
#' Generate jackknife subsamples. For data with \eqn{n} elements,
#' the jackknife generates \eqn{n} subsamples, each with one element
#' deleted. The generalized (delete-\eqn{p}, block) jackknife generates
#' subsamples by deleting \code{p} observations.
#'
#' @template param-expr
#' @param p The number of elements to delete. \code{p = 1} is the ordinary
#'    (delete-one) jackknife, while \code{p > 1} is the block (delete-\eqn{p})
#'    jackknife.
#' @template param-n
#' @template param-extractor
#' @param ... Arguments passed to methods
#' @templateVar numrows \code{combn(n, p)} (\code{n} if \code{p = 1})
#' @templateVar f jackknife
#' @templateVar fn jackknife_n
#' @template return_lazy_sample_df
#' @references
#' \itemize{
#' \item{Davison, A. C. & Hinkley, D. V. (1997) Bootstrap Methods and Their Applications. Cambridge University Press, Cambridge. ISBN 0-521-57391-2}
#' \item{Tukey, J. W. (1958). "Bias and confidence in not quite large samples". The Annals of Mathematical Statistics.  \href{https://dx.doi.org/10.1214/aoms/1177706647}{doi:10.1214/aoms/1177706647}.}
#' \item{Efron, B.; Stein, C. (May 1981). "The Jackknife Estimate of Variance". The Annals of Statistics. \href{https://dx.doi.org/10.1214/aos/1176345462}{doi:10.1214/aos/1176345462}.}
#' }
#' @export
jackknife <- function(expr, p = 1L, n = NULL, extractor = NULL) {
  expr <- enquo(expr)
  n <- n %||% idx_len(expr)
  out <- jackknife_n(n, p = p)
  out[["sample"]] <- lazy_sample_lst(UQ(expr), out[["sample"]], extractor = extractor)
  out
}

#' @rdname jackknife
#' @export
#' @importFrom purrr map_df
#' @importFrom dplyr bind_rows
jackknife_n <- function(n, p = 1L) {
  # this is copy/pasted from crossv_lpo_n
  # I could have called crossv_lpo_n and manipulated the returned
  # data frame, but don't for efficiency (since that would add training data)
  n <- as.integer(n)
  assert_that(is_pos_scalar_integer(n))
  p <- as.integer(p)
  assert_that(is_pos_scalar_integer(p))
  idx <- seq_len(n)
  if (p == 1) {
    deleted <- as.list(idx)
  } else {
    deleted <- combn(idx, p, simplify = FALSE)
  }
  tibble(sample = map(deleted, function(i) base::setdiff(idx, i)))
}
