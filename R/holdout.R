#' Generate cross-validation test-training pairs
#'
#' \code{holdout_frac} splits the data so that proportion \code{p} is in test
#' set and \code{1 - p} is in the training set. Likewise, \code{holdout_n}
#' splits the data so that \code{n} elements are in the test set and the
#' remainder are in the training set.
#'
#' @param p Fraction in the test set.
#' @param n Number in the test set.
#' @param k Number of test/train splits to generate.
#' @param x A data table or vector.
#' @param shuffle If \code{TRUE}, the observations are randomly assigned to the
#'   test and training sets. If \code{FALSE}, then the last \code{p} or
#'   \code{n} of the observations are assigned to the training set, and
#'   the remainder of the observations are assigned to the test set.
#' @param stratify If \code{TRUE}, then test-train splits are within each
#'   code group, so that the final test and train subsets have approximately
#'   equal proportions of each group. If \code{FALSE}, the the test-train splits
#'   splits groups into the testing and training sets.
#' @param ... Arguments passed to methods.
#' @return A data frame
#' @export
holdout_frac <- function(x, ...) {
  UseMethod("holdout_frac")
}

#' @rdname holdout_frac
#' @export
holdout_frac.data.frame <- function(x, p = 0.3, k = 1, shuffle = TRUE, ...) {
  assert_that(is.number(p) && p >= 0 && p <= 1)
  assert_that(is.number(k) && k > 0)
  assert_that(is.flag(shuffle))
  idx <- seq_len(nrow(x))
  res <- holdout_frac(idx, p = p, k = k, shuffle = shuffle, ...)
  to_crossv_df(res, x)
}

#' @rdname holdout_frac
#' @export
holdout_frac.grouped_df <- function(x, p = p, k = k,
                                    shuffle = shuffle,
                                    stratify = FALSE,
                                    ...) {
  assert_that(is.number(p) && p >= 0 && p <= 1)
  assert_that(is.number(k) && k > 0)
  assert_that(is.flag(shuffle))
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(x)
  if (stratify) {
    # since n varies with group, need to
    grp_sz <- round(purrr::map_int(idx, length) * p)
    f <- function(.id) {
      x <- purrr::map2_df(idx, grp_sz, function(i, n) {
        holdout_n(i, n = n, k = 1L, shuffle = shuffle)
      })
      x[[".id"]] <- .id
      x[["test"]] <- flatten_int(x[["test"]])
      x[["train"]] <- flatten_int(x[["train"]])
      x
    }
    x <- purrr::map_df(seq_len(k), f)
  } else {
    n <- round(p * nrow(x))
    res <- holdout_n(x, n = n, k = k, stratify = FALSE, ...)
  }
  to_crossv_df(res, x)
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
  assert_that(is.number(n) && n >= 0 && n <= nrow(x))
  assert_that(is.number(k) && k > 0)
  assert_that(is.flag(shuffle))
  idx <- seq_len(nrow(x))
  holdout_n(idx, n = n, k = k, shuffle = shuffle, ...)
}

#' @rdname holdout_frac
#' @export
holdout_n.grouped_df <- function(x, n = 1L, k = 1L, shuffle = TRUE,
                                 stratify = FALSE, ...) {
  assert_that(is.number(n) && n >= 0)
  assert_that(is.number(k) && k >= 0)
  assert_that(is.flag(shuffle))
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(x)
  if (stratify) {
    f <- function(.id) {
      d <- purrr::map_df(idx, function(i) {
        holdout_n(i, n = n, k = 1L, shuffle = shuffle)
      })
      d[[".id"]] <- .id
      d[["test"]] <- flatten_int(d[["test"]])
      d[["train"]] <- flatten_int(d[["train"]])
      d
    }
    res <- purrr::map_df(seq_len(k), f)
  } else {
    res <- dplyr::mutate_(holdout_n(idx, n = n, k = k, shuffle = shuffle),
                         train = ~ map(train, flatten_int),
                         test = ~ map(test, flatten_int))
  }
  to_crossv_df(res, x)
}

holdout_n_ <- function(idx, n, shuffle = TRUE) {
  if (shuffle) {
    idx <- sample(idx, length(idx), replace = FALSE)
  }
  test_idx <- utils::tail(idx, n)
  list(train = setdiff(idx, test_idx), test = test_idx)
}

#' @rdname holdout_frac
#' @importFrom purrr rerun transpose
#' @export
holdout_n.default <- function(x, n, k = 1, shuffle = TRUE, ...) {
  res <- as_tibble(transpose(rerun(k, holdout_n_(x, n, shuffle = shuffle))))
  res[[".id"]] <- id(nrow(x))
  res
}

to_crossv_df <- function(x, data) {
  x[["train"]] <- resample_lst(data, x[["train"]])
  x[["test"]] <- resample_lst(data, x[["test"]])
  x
}

crossv_df <- function(.data, ...) {
  structure(.data, class = c("crossv_df", class(.data)))
}

resample_df <- function(.data, ...) {
  structure(.data, class = c("resample_df", class(.data)))
}
