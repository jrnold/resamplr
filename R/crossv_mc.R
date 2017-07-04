#' Generate K cross-validation test-training pairs
#'
#' \code{crossv_mc} generates random train/test pairs where either proportion \code{frac} of the elements are in the test set and \code{1 - frac} elements are in the training set, or \code{size} elements are in the test set and \code{n - size} elements are in the training set.
#'

#' @template param-expr
#' @template param-n
#' @param frac A numeric scalar between 0 and 1. The fraction of elements in the test set.
#' @param size An integer scalar. The number of elements in the test set. If \code{size} is non-\code{NULL}, then it takes preference over \code{frac}.
#' @param K Number of test/train splits to generate.
#' @param prob Probability weight that an element is in the \code{test} set.
#' @template param-extractor
#' @param ... Arguments passed to \code{crossv_mc_n}.
#' @seealso The \pkg{modelr} function \code{\link[modelr]{crossv_mc}}.
#' @inherit crossv_kfold references
#' @family cross-validation functions
#' @templateVar numrows \code{K}
#' @templateVar f crossv_mc
#' @templateVar fn crossv_mc_n
#' @template return_crossv
#' @example inst/examples/ex-crossv_mc.R
#' @export
crossv_mc <- function(expr, ..., n = NULL, extractor = NULL) {
  expr <- enquo(expr)
  n <- n %||% idx_len(expr)
  out <- crossv_mc_n(n, ...)
  for (i in c("train", "test")) {
    out[[i]] <- lazy_sample_lst(UQ(expr), out[[i]], extractor = extractor)
  }
  out
}

holdout <- function(n, size, shuffle = TRUE, prob = NULL) {
  if (shuffle) {
    utils::head(sample.int(n, size, replace = FALSE, prob = prob))
  } else {
    seq_len(min(size, n))
  }
}

# using size for the name of the heldout set and n for the size of the
# vector is confusing, but more consistent with the names of the other functions
# x as the name as the first arg doesn't make sense if it is a single integer.

#' @rdname crossv_mc
#' @export
crossv_mc_n <- function(n, K = 10L, frac = 0.3, size = NULL, prob = NULL) {
  n <- as.integer(n)
  assert_that(is_pos_scalar_integer(n))
  K <- as.integer(K)
  assert_that(is_pos_scalar_integer(K))
  # checks
  if (!is.null(frac)) {
    assert_that(is_scalar_proportion(frac, strict = TRUE))
    # ensure that frac always produces a size that
    # produces non-empty training and test sets.
    size <- max(min(round(frac * n), n - 1L), 1L)
  } else {
    if (!is.null(size)) {
      assert_that(is_pos_scalar_integer(size))
      assert_that(size < n)
    } else {
      stop("Either `size` or `frac` must be specified", call. = FALSE)
    }
  }
  if (!is.null(prob)) {
    assert_that(is.numeric(prob))
    assert_that(all(prob >= 0 & prob <= 1))
  }
  test <- rerun(K, holdout(n, size, shuffle = TRUE, prob = prob))
  idx <- seq_len(n)
  train <- map(test, function(i) base::setdiff(idx, i))
  tibble(train = train, test = test)
}
