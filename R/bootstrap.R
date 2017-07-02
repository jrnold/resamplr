#' Generate bootstrap replicates
#'
#' Generate bootstrap replicates. In addition to the ordinary bootstrap,
#' it supports weighted bootstrap (\code{weights}),
#' Bayesian bootstrap (\code{bayes}), cluster bootstrap (if \code{data} is a
#' grouped data frame and \code{groups = TRUE}), and stratified bootstrap
#' (if \code{data} is grouped data frame and \code{stratify = TRUE}).
#'
#' @param expr A quosure
#' @param R Number of replicates to sample
#' @param n Number of elements to sample from. If \code{NULL}, then
#'   \code{n = len_idx(expr)}.
#' @param m Number of observations in the replicates. This can be set to values \code{m != n} for sub-sampling or oversampling.
#' @param extractor See \code{\link{lazy_sample}}.
#'
#'
#' @param replace Logical scalar. If \code{TRUE}, then sample with replacement, otherwise sample without replacement.
#' @param weights Observation level weights to use for a weighted bootstrap. If \code{bayesian = TRUE}, then the sum of these weights will adjust the concentration of the Dirichlet prior.
#' @seealso The \pkg{boot} function \code{\link[boot]{boot}} which is the
#'   canonical R bootstrap implementation.
#'   The \pkg{modelr} function \code{\link[modelr]{bootstrap}} also implements a lazy bootstrap.
#' @family boot
#' @param bayesian If \code{TRUE}, a Bayesian bootstrap is used.
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
                      m = NULL,
                      replace = TRUE,
                      n = NULL,
                      extractor = NULL) {
  expr <- as_quosure(expr)
  n <- n %||% idx_len(expr)
  if (rlang::is_character(weights)) {
    weights <- (eval_tidy(expr))[[weights]]
  } else if (rlang::is_symbol(weights)) {
    weights <- `$`(eval_tidy(expr), weights)
  }
  out <- bootstrap_n(n, R = R, weights = weights, bayesian = bayesian,
                     replace = replace, m = m)
  out[["sample"]] <- lazy_sample_lst(expr, out[["sample"]], extractor = extractor)
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
                        m = NULL,
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
  m <- as.integer(m %||% n)
  assert_that(is_pos_scalar_integer(m))

  # resample weights
  if (bayesian) {
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
