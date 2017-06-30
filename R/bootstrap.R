#' Generate bootstrap replicates
#'
#' Generate bootstrap replicates. In addition to the ordinary bootstrap,
#' it supports weighted bootstrap (\code{weights}),
#' Bayesian bootstrap (\code{bayes}), cluster bootstrap (if \code{data} is a
#' grouped data frame and \code{groups = TRUE}), and stratified bootstrap
#' (if \code{data} is grouped data frame and \code{stratify = TRUE}).
#'
#' @param data A data frame
#' @param R Number of replicates to sample
#' @param m Number of observations in the replicates.
#' @param ... Passed to methods
#' @param stratify Resample within groups (stratified bootstrap)
#' @param groups Resample groups (clustered bootstrap)
#' @param weights Observation level weights. This must be the name of a column
#'    in \code{data}.
#' @param weight_groups If \code{TRUE} and \code{groups = TRUE},
#'    then sample groups using group-level weights.
#'    If \code{weights} is \code{NULL}, then weight groups by the number
#'    of observations in each. If \code{weights}, is not-\code{NULL}, then
#'    use the sum of the observation level weights in each group as the
#'    group-level weight.
#' @param weight_within If \code{TRUE} and \code{stratify = TRUE}, then
#'    use observation level weights to sample observations within each group.
#'
#' @seealso The \pkg{boot} function \code{\link[boot]{boot}} which is the
#'   canonical R bootstrap implementation. This function produces slightly different
#'   output and has more options than the \pkg{modelr} function
#'   \code{\link[modelr]{bootstrap}}.
#' @param bayes If \code{TRUE}, a Bayesian bootstrap is used.
#' @references
#' \itemize{
#' \item{Angelo Canty and Brian Ripley (2016). boot: Bootstrap R (S-Plus) Functions. R package version 1.3-18.}
#' \item{Davison, A. C. & Hinkley, D. V. (1997) Bootstrap Methods and Their Applications. Cambridge University Press, Cambridge. ISBN 0-521-57391-2}
#' }
#' @export
#' @example inst/examples/ex-bootstrap.R
bootstrap <- function(data,
                      R = 1L,
                      n = NULL,
                      extractor = NULL,
                      ...) {
  data <- as_quosure(data)
  n <- n %||% idx_len(data)
  out <- bootstrap_n(n, R = R, ...)
  out[["sample"]] <- resample_lst(data, out[["sample"]],
                                  extractor = extractor)
  out
}



#' @rdname bootstrap
#' @export
#' @importFrom purrr rerun
#' @importFrom assertthat assert_that is.flag is.number
#' @importFrom tibble tibble
bootstrap_n <- function(n,
                        R = 1L,
                        weights = NULL,
                        bayes = FALSE,
                        m = NULL,
                        replace = TRUE) {
  assert_that(is.number(n))
  assert_that(is.number(R))
  assert_that(is.flag(bayes))
  assert_that(is.null(weights) || is.numeric(weights))
  m <- m %||% n
  assert_that(is.number(m))
  # resample weights
  if (bayes) {
    # a Dirichlet sample can be created by drawing indep gamma
    # don't need to normalize these to sum to 1 since sample.int doesn't care
    weights <- rerun(R, stats::rgamma(n, shape = weights %||% 1))
    f <- function(prob) {
      sample.int(n = n, size = m, replace = replace, prob = prob)
    }
    tibble(sample = map(weights, f))
  } else {
    tibble(sample = rerun(R, sample.int(n, size = m, replace = replace,
                                        prob = weights)))
  }
}
