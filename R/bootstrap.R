#' Generate bootstrap replicates
#'
#' Generate bootstrap replicates. In addition to the ordinary bootstrap,
#' it supports weighted bootstrap (\code{weights}),
#' Bayesian bootstrap (\code{bayes}), cluster bootstrap (if \code{data} is a
#' grouped data frame and \code{groups = TRUE}), and stratified bootstrap
#' (if \code{data} is grouped data frame and \code{stratify = TRUE}).
#'
#' @template param-expr
#' @template param-n
#' @param R Number of replicates to sample
#' @param frac Proportion of observations to sample in each replicates. This can be set to values \code{frac < 1} for sub-sampling or \code{frac > 1} for oversampling.
#' @param size Number of observations to sample in each replicate. Set \code{size != n} to under- or over-sample. If \code{size} is not \code{NULL}, then \code{frac} is ignored.
#' @template param-extractor
#' @param replace Logical scalar. If \code{TRUE}, then sample with replacement, otherwise sample without replacement.
#' @param weights Observation level weights to use for a weighted bootstrap. If \code{bayesian = TRUE}, then the sum of these weights will adjust the concentration of the Dirichlet prior.
#' @param bayesian If \code{TRUE}, a Bayesian bootstrap is used.
#' @templateVar numrows \code{R}
#' @templateVar f bootstrap
#' @templateVar fn bootstrap_n
#' @template return_lazy_sample_df
#'
#' @seealso The \pkg{boot} function \code{\link[boot]{boot}} which is the
#'   canonical R bootstrap implementation.
#'   The \pkg{modelr} function \code{\link[modelr]{bootstrap}} also implements a lazy bootstrap.

#' @family bootstrap methods
#' @example inst/examples/ex-bootstrap.R
#' @references
#' \itemize{
#' \item{Angelo Canty and Brian Ripley (2016). boot: Bootstrap R (S-Plus) Functions. R package version 1.3-18.}
#' \item{Davison, A. C. & Hinkley, D. V. (1997) Bootstrap Methods and Their Applications. Cambridge University Press, Cambridge. ISBN 0-521-57391-2}
#' }
#' @export
bootstrap <- function(expr,
                      R = 1L,
                      weights = NULL,
                      bayesian = FALSE,
                      size = NULL,
                      frac = 1,
                      replace = TRUE,
                      n = NULL,
                      extractor = NULL) {
  expr <- enquo(expr)
  n <- n %||% idx_len(expr)
  out <- bootstrap_n(n, R = R, weights = weights, bayesian = bayesian,
                     replace = replace, size = size, frac = frac)
  out[["sample"]] <- lazy_sample_lst(UQ(expr), out[["sample"]], extractor = extractor)
  out
}


#' @rdname bootstrap
#' @family bootstrapping functions
#' @export
#' @importFrom purrr rerun
#' @importFrom assertthat assert_that is.flag is.number
#' @importFrom tibble tibble
bootstrap_n <- function(n,
                        R = 1L,
                        weights = NULL,
                        bayesian = FALSE,
                        size = NULL,
                        frac = NULL,
                        replace = TRUE) {
  n <- as.integer(n)
  assert_that(is_pos_scalar_integer(n))
  R <- as.integer(R)
  assert_that(is_pos_scalar_integer(n))
  assert_that(is.flag(bayesian))
  if (!is.null(weights)) {
    assert_that(is.numeric(weights))
    assert_that(length(weights) %in% c(1, n))
    assert_that(all(weights >= 0))
  }
  if (!is.null(frac)) {
    assert_that(is_scalar_double(frac) && frac > 0)
    size <- round(n * frac)
  }
  size <- max(1, size)
  assert_that(is_pos_scalar_integer(size))

  # resample weights
  if (bayesian) {
    # a Dirichlet sample can be created by drawing indep gamma
    # don't need to normalize these to sum to 1 since sample.int doesn't care
    weights <- rerun(R, stats::rgamma(n, shape = weights %||% 1))
    f <- function(prob) {
      sample.int(n = n, size = size, replace = replace, prob = prob)
    }
    tibble(sample = map(weights, f))
  } else {
    tibble(sample = rerun(R, sample.int(n, size = size, replace = replace,
                                        prob = weights)))
  }
}
