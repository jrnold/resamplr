#' Generate K cross-validation test-training pairs
#'
#' \code{holdout_frac} splits the data so that proportion \code{size} is in test
#' set and \code{1 - size} is in the training set. Likewise, \code{holdout_n}
#' splits the data so that \code{size} elements are in the test set and the
#' remainder are in the training set.
#'
#' @param data A data frame
#' @param size For \code{holdout_n}, the number of elements in the test set.
#'   For \code{holdout_frac}, the fraction of elements in test set.
#' @param K Number of test/train splits to generate.
#' @param shuffle If \code{TRUE}, the observations are randomly assigned to the
#'   test and training sets. If \code{FALSE}, then the \emph{first} \code{size}
#'   elements are assigned to the test set, and
#'   the remainder of the observations are assigned to the training set.
#' @param stratify If \code{TRUE}, then test-train splits are within each
#'   code group, so that the final test and train subsets have approximately
#'   equal proportions of each group. If \code{FALSE}, the the test-train splits
#'   splits groups into the testing and training sets.
#' @param prob Probability weight that an element is in the \code{test} set.
#'   If non-\code{NULL} this is numeric vector with \code{nrow(data)} row weights
#'   if \code{data} is a data frame or a grouped data frame and
#'   \code{stratify = TRUE}, or \code{n_groups(data)} group weights if \code{data} is
#'   a grouped data frame and \code{stratify = FALSE}.
#' @param ... Arguments passed to methods.
#' @seealso This function is similar to the \pkg{modelr} function
#'   \code{\link[modelr]{crossv_mc}}, but with more features.
#' @templateVar numrows \code{K} rows and
#' @template return_crossv_df
#' @export
#' @example inst/examples/ex-holdout.R
holdout_frac <- function(data, ...) {
  UseMethod("holdout_frac")
}

# Note: I wanted to use the same names as sample_n and sample_frac in dplyr
# for consistency

#' @describeIn holdout_frac Split rows in a data frame into test and training
#'   data sets.
#' @export
holdout_frac.data.frame <- function(data, size = 0.3, K = 1L, shuffle = TRUE,
                                    prob = NULL, ...) {
  # TODO: should I allow for size %in% c(0, 1) which will give size 0
  assert_that(is.number(size) && size >= 0 && size <= 1)
  assert_that(is.number(K) && K >= 1)
  assert_that(is.flag(shuffle))
  assert_that(is.null(prob) || (is.numeric(prob) &&
                                  length(prob) %in% c(1, nrow(data))))
  res <- holdout_frac_(nrow(data), size = size, K = K, shuffle = shuffle, prob=prob)
  to_crossv_df(res, data)
}

#' @describeIn holdout_frac Splits within each group of a grouped data frame
#'   into test and training sets if \code{stratify = FALSE}. This ensures that the test and training
#'   sets will have approximately equal proportions of each group in the training
#'   and test sets. If \code{stratify = TRUE}, then the groups are split into test and training sets.
#' @importFrom dplyr n_groups group_size
#' @export
holdout_frac.grouped_df <- function(data, size = 0.3, K = 1L, shuffle = TRUE,
                                    stratify = FALSE, prob = NULL,
                                    ...) {
  assert_that(is.numeric(size) && all(size >= 0) && all(size <= 1))
  assert_that(is.number(K) && K >= 1)
  assert_that(is.flag(shuffle))
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(data)
  if (stratify) {
    assert_that(length(size) %in% c(1L, n_groups(data)))
    if (length(size) == 1) size <- rep(size, n_groups(data))
    size <- round(size * group_size(data))
  } else {
    assert_that(length(size) == 1)
    size <- round(size * length(idx))
  }
  holdout_n(data, size = size, K = K, shuffle = shuffle, stratify = stratify,
            prob = prob, ...)
}

#' @rdname holdout_frac
#' @export
holdout_n <- function(data, ...) {
  UseMethod("holdout_n")
}

#' @describeIn holdout_frac Split rows in a data frame into test and training
#'   data sets.
#' @export
holdout_n.data.frame <- function(data, size = 1L, K = 1L, shuffle = TRUE,
                                 prob = NULL, ...) {
  # TODO: should I check that size <= nrow(data) ? or just return size 0 sets
  assert_that(is.number(size) && size >= 1)
  assert_that(is.number(K) && K >= 1)
  assert_that(is.flag(shuffle))
  assert_that(is.null(prob) || (is.numeric(prob) &&
                                  length(prob) %in% c(1L, nrow(data))))
  res <- holdout_n_(nrow(data), size = size, K = K, shuffle = shuffle,
                    prob = prob)
  to_crossv_df(res, data)
}

#' @describeIn holdout_frac Splits within each group of a grouped data frame
#'   into test and training sets if \code{stratify = FALSE}. This ensures that the test and training
#'   sets will have approximately equal proportions of each group in the training
#'   and test sets. If \code{stratify = TRUE}, then the groups are split into test and training sets.
#' @importFrom dplyr n_groups
#' @export
holdout_n.grouped_df <- function(data, size = 1L, K = 1L, shuffle = TRUE,
                                 stratify = FALSE, prob = NULL, ...) {
  assert_that(is.numeric(size) && all(size >= 1))
  size <- as.integer(size)
  assert_that(is.number(K) && K >= 1)
  assert_that(is.flag(shuffle))
  assert_that(is.flag(stratify))
  assert_that(is.null(prob) || is.numeric(prob))
  idx <- group_indices_lst(data)
  if (stratify) {
    # allow for different sizes for each group
    assert_that(length(size) %in% c(1L, n_groups(data)))
    if (length(size) == 1) size <- rep(size, n_groups(data))
    if (!is.null(prob)) {
      assert_that(length(prob) %in% nrow(data))
      prob <- split(prob, dplyr::group_indices(data))
    } else {
      prob <- map(seq_len(n_groups(data)), ~ NULL)
    }
    f <- function(idx, size, prob) {
      mutate_(holdout_n_(length(idx), size = size, K = K, shuffle = shuffle),
              train = ~ map(train, function(i) idx[i]),
              test = ~ map(test, function(i) idx[i]))
    }
    res <- summarise_(group_by_(purrr::pmap_df(list(idx = idx, size = size,
                                                    prob = prob), f), ".id"),
                      train = ~ list(flatten_int(train)),
                      test = ~ list(flatten_int(test)))
  } else {
    assert_that(length(size) == 1)
    if (!is.null(prob)) assert_that(length(prob) %in% n_groups(data))
    res <- mutate_(holdout_n_(length(idx), size = size, K = K,
                              shuffle = shuffle, prob = prob),
                   train ~ map(train, function(i) flatten_int(idx[i])),
                   test ~ map(test, function(i) flatten_int(idx[i])))
  }
  to_crossv_df(res, data)[, c("train", "test", ".id")]
}

# using size for the name of the heldout set and n for the size of the
# vector is confusing, but more consistent with the names of the other functions
# x as the name as the first arg doesn't make sense if it is a single integer.
holdout_n_ <- function(n, size = 1L, K = 1L, shuffle = TRUE, prob = NULL) {
  f <- function(i) {
    if (shuffle) {
      idx <- sample.int(n, n, replace = FALSE, prob = prob)
    } else {
      idx <- seq_len(n)
    }
    test_idx <- utils::head(idx, size)
    tibble(train = list(setdiff(idx, test_idx)),
           test = list(test_idx),
           .id = i)
  }
  map_df(seq_len(K), f)
}

holdout_frac_ <- function(n, size = 0.3, K = 1L, shuffle = TRUE, prob = NULL) {
  holdout_n_(n, size = size * n, K = K, shuffle = shuffle, prob = prob)
}
