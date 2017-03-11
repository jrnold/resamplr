#' Generate cross-validation test-training pairs
#'
#' \code{holdout_frac} splits the data so that proportion \code{p} is in test
#' set and \code{1 - p} is in the training set. Likewise, \code{holdout_n}
#' splits the data so that \code{n} elements are in the test set and the
#' remainder are in the training set.
#'
#'
#' @param x A data frame
#' @param p Fraction (proportion) of elements in the test set.
#' @param n Number of elements in the test set.
#' @param k Number of test/train splits to generate.
#' @param shuffle If \code{TRUE}, the observations are randomly assigned to the
#'   test and training sets. If \code{FALSE}, then the last \code{p} or
#'   \code{n} of the observations are assigned to the test set, and
#'   the remainder of the observations are assigned to the training set.
#' @param stratify If \code{TRUE}, then test-train splits are within each
#'   code group, so that the final test and train subsets have approximately
#'   equal proportions of each group. If \code{FALSE}, the the test-train splits
#'   splits groups into the testing and training sets.
#' @param ... Arguments passed to methods.
#' @return A data frame with \code{k} rows and columns
#' \describe{
#' \item{\code{train}}{A list of \code{\link{resample}} objects representing
#'   the training sets}
#' \item{\code{test}}{A list of \code{\link{resample}} objects representing the
#'   test sets}
#' \item{\code{.id}}{An integer vector of ids of the replicates}
#' }
#' @export
#' @example inst/examples/ex-holdout.R
holdout_frac <- function(x, ...) {
  UseMethod("holdout_frac")
}

#' @rdname holdout_frac
#' @export
holdout_frac.data.frame <- function(x, p = 0.3, k = 1, shuffle = TRUE, ...) {
  assert_that(is.number(p) && p >= 0 && p <= 1)
  n <- round(p * length(x))
  holdout_n(x, n = n, k = k, shuffle = shuffle)
}

#' @rdname holdout_frac
#' @export
holdout_frac.grouped_df <- function(x, p = p, k = k,
                                    shuffle = TRUE,
                                    stratify = FALSE,
                                    ...) {
  assert_that(is.number(p) && p >= 0 && p <= 1)
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(x)
  if (stratify) {
    f <- function(g) {
      n <- p * length(g)
      mutate_(holdout_n_(length(g), n = n, k = k, shuffle = shuffle),
              train = ~ map(train, function(i) g[i]),
              test = ~ map(test, function(i) g[i]))
    }
    res <- summarise_(group_by_(map_df(idx, f), ".id"),
                      train = ~ list(flatten_int(train)),
                      test = ~ list(flatten_int(test)))
    to_crossv_df(res, x)
  } else {
    n <- round(p * length(idx))
    res <- holdout_n(x, n = n, k = k, stratify = FALSE, ...)
  }
}

#' @rdname holdout_frac
#' @export
holdout_frac.default <- function(x, p = 0.3, k = 1L, shuffle = TRUE, ...) {
  n <- round(p * length(x))
  holdout_n(x, n, k = k, shuffle = shuffle)
}

#' @rdname holdout_frac
#' @export
holdout_n <- function(x, ...) {
  UseMethod("holdout_n")
}

#' @rdname holdout_frac
#' @export
holdout_n.data.frame <- function(x, n = 1L, k = 1L, shuffle = TRUE, ...) {
  res <- holdout_n_(nrow(x), n = n, k = k, shuffle = shuffle, ...)
  to_crossv_df(res, x)
}

#' @rdname holdout_frac
#' @export
holdout_n.grouped_df <- function(x, n = 1L, k = 1L, shuffle = TRUE,
                                 stratify = FALSE, ...) {
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(x)
  if (stratify) {
    f <- function(g) {
      mutate_(holdout_n_(length(g), n = n, k = k, shuffle = shuffle),
              train = ~ map(train, function(i) g[i]),
              test = ~ map(test, function(i) g[i]))
    }
    res <- summarise_(group_by_(map_df(idx, f), ".id"),
                      train = ~ list(flatten_int(train)),
                      test = ~ list(flatten_int(test)))
  } else {
    res <- mutate_(holdout_n_(length(idx), n = n, k = k, shuffle = shuffle),
                   train ~ map(train, function(i) flatten_int(idx[i])),
                   test ~ map(test, function(i) flatten_int(idx[i])))
  }
  to_crossv_df(res, x)[ , c("train", "test", ".id")]
}

holdout_n_ <- function(x, n = 1L, k = 1L, shuffle = TRUE) {
  assert_that(is.number(x) && x >= 1)
  assert_that(is.number(n) && n >= 0)
  assert_that(is.number(n) && k >= 0)
  assert_that(is.flag(shuffle))
  f <- function(i) {
    if (shuffle) {
      idx <- sample.int(x, x, replace = FALSE)
    } else {
      idx <- seq_len(x)
    }
    test_idx <- utils::tail(idx, n)
    tibble(train = list(setdiff(idx, test_idx)),
           test = list(test_idx),
           .id = i)
  }
  map_df(seq_len(k), f)
}
