#' Generate test/train resample objects
#'
#' @param test,train Integer vectors with the indexes of the test
#'   and training samples.
#'   In \code{resample_holdout.data.frame}, \code{test} and \code{train}
#'   are the indexes of observations.
#'   In \code{resample_holdout.grouped_df}, \code{test} and \code{train}
#'   are group numbers.
#'   One of \code{test} or \code{train} must be non-\code{NULL}.
#' @param data A data table
#' @param ... Arguments passed to methods
#' @return A named list of two \code{\link[modelr]{resample}} objects
#'   for the "test" and "train" sets.
#' @export
resample_holdout <- function(data, ...) {
  UseMethod("resample_holdout")
}

#' @export
resample_holdout.data.frame <- function(data, test = NULL, train = NULL, ...) {
  if (is.null(test) && is.null(train)) {
    stop("Either test or train must be non-null", call. = FALSE)
  }
  test <- test %||% setdiff(seq_len(nrow(data)), train)
  train <- train %||% setdiff(seq_len(nrow(data)), test)
  list(train = resample(data, train), test = resample(data, test))
}

#' @describeIn resample_holdout Resamples
#' @export
resample_holdout.grouped_df <- function(data, test = NULL, train = NULL, ...) {
  if (is.null(test) && is.null(train)) {
    stop("Either test or train must be non-null", call. = FALSE)
  } else if (is.null(test)) {
    idx <- split_idx_by_group(data, train)
  } else if (is.null(train)) {
    idx <- rev(split_idx_by_group(data, test))
  } else {
    idx <- list(get_group_indexes_int(data, train),
                get_group_indexes_int(data, test))
  }
  purrr::set_names(map(idx, function(i) resample(data, i)), c("train", "test"))
}
