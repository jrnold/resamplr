#' Generate cross-validation test-training pairs
#'
#' \code{holdout_frac} splits the data so that proportion \code{size} is in test
#' set and \code{1 - size} is in the training set. Likewise, \code{holdout_n}
#' splits the data so that \code{size} elements are in the test set and the
#' remainder are in the training set.
#'
#'
#' @param x A data frame
#' @param size For \code{holdout_n}, the number of elements in the test set.
#'   For \code{holdout_frac}, the fraction of elements in test set.
#' @param k Number of test/train splits to generate.
#' @param shuffle If \code{TRUE}, the observations are randomly assigned to the
#'   test and training sets. If \code{FALSE}, then the last \code{size}
#'   elements are assigned to the test set, and
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

# Note: I wanted to use the same names as sample_n and sample_frac in dplyr
# for consistency

#' @rdname holdout_frac
#' @export
holdout_frac.data.frame <- function(x, size = 0.3, k = 1, shuffle = TRUE, ...) {
  # TODO: should I allow for size %in% c(0, 1) which will give size 0
  assert_that(is.number(size) && size >= 0 && size <= 1)
  assert_that(is.number(k) && k >= 1)
  assert_that(is.flag(shuffle))
  res <- holdout_frac_(nrow(x), size = size, k = k, shuffle = shuffle)
  to_crossv_df(res, x)
}

#' @rdname holdout_frac
#' @importFrom dplyr n_groups
#' @export
holdout_frac.grouped_df <- function(x, size = 0.3, k = k, shuffle = TRUE,
                                    stratify = FALSE,
                                    ...) {
  assert_that(is.numeric(size) && all(size >= 0) && all(size <= 1))
  assert_that(is.number(k) && k >= 1)
  assert_that(is.flag(shuffle))
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(x)
  if (stratify) {
    # allow for either constant of different heldout sizes per group
    assert_that(length(size) %in% c(1, n_groups(x)))
    if (length(size) == 1) size <- rep(size, n_groups(x))
    f <- function(g, size) {
      mutate_(holdout_frac_(length(g), size = size, k = k, shuffle = shuffle),
              train = ~ map(train, function(i) g[i]),
              test = ~ map(test, function(i) g[i]))
    }
    res <- summarise_(group_by_(map2_df(idx, size, f), ".id"),
                      train = ~ list(flatten_int(train)),
                      test = ~ list(flatten_int(test)))
    res <- to_crossv_df(res, x)
  } else {
    assert_that(length(size) == 1)
    n <- round(size * length(idx))
    res <- holdout_n(x, size = n, k = k, stratify = FALSE, ...)
  }
  res[ , c("train", "test", ".id")]
}

#' @rdname holdout_frac
#' @export
holdout_n <- function(x, ...) {
  UseMethod("holdout_n")
}

#' @rdname holdout_frac
#' @export
holdout_n.data.frame <- function(x, size = 1L, k = 1L, shuffle = TRUE, ...) {
  # TODO: should I check that size <= nrow(x) ? or just return size 0 sets
  assert_that(is.number(size) && size >= 1)
  assert_that(is.number(k) && k >= 1)
  assert_that(is.flag(shuffle))
  res <- holdout_n_(nrow(x), size = size, k = k, shuffle = shuffle)
  to_crossv_df(res, x)
}

#' @rdname holdout_frac
#' @importFrom dplyr n_groups
#' @export
holdout_n.grouped_df <- function(x, size = 1L, k = 1L, shuffle = TRUE,
                                 stratify = FALSE, ...) {
  assert_that(is.numeric(size) && all(size >= 1))
  size <- as.integer(size)
  assert_that(is.number(k) && k >= 1)
  assert_that(is.flag(shuffle))
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(x)
  if (stratify) {
    # allow for different sizes for each group
    assert_that(length(size) %in% c(1L, n_groups(x)))
    if (length(size) == 1) size <- rep(size, n_groups(x))
    f <- function(g, size) {
      mutate_(holdout_n_(length(g), size = size, k = k, shuffle = shuffle),
              train = ~ map(train, function(i) g[i]),
              test = ~ map(test, function(i) g[i]))
    }
    res <- summarise_(group_by_(map2_df(idx, size, f), ".id"),
                      train = ~ list(flatten_int(train)),
                      test = ~ list(flatten_int(test)))
  } else {
    assert_that(length(size) == 1)
    res <- mutate_(holdout_n_(length(idx), size = size, k = k,
                              shuffle = shuffle),
                   train ~ map(train, function(i) flatten_int(idx[i])),
                   test ~ map(test, function(i) flatten_int(idx[i])))
  }
  to_crossv_df(res, x)[ , c("train", "test", ".id")]
}

# using size for the name of the heldout set and n for the size of the
# vector is confusing, but more consistent with the names of the other functions
# x as the name as the first arg doesn't make sense if it is a single integer.
holdout_n_ <- function(n, size = 1L, k = 1L, shuffle = TRUE) {
  f <- function(i) {
    if (shuffle) {
      idx <- sample.int(n, n, replace = FALSE)
    } else {
      idx <- seq_len(n)
    }
    test_idx <- utils::tail(idx, size)
    tibble(train = list(setdiff(idx, test_idx)),
           test = list(test_idx),
           .id = i)
  }
  map_df(seq_len(k), f)
}

holdout_frac_ <- function(n, size = 0.3, k = 1L, shuffle = TRUE) {
  holdout_n_(n, size = size * n, k = k, shuffle = shuffle)
}
