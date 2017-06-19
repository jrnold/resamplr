#' Generate subsamples from indices
#'
#' Generate subsamples from manually specified indices of observations in each
#' subsample.
#'
#' @param data An object
#' @param samples A list of integer vectors or an integer vector, with the indices in each subsample.
#' @param ... Arguments passed to methods
#' @templateVar numrows rows for each subsample and
#' @template return_resample
#' @example inst/examples/ex-resample.R
#' @export
resample_idx <- function(data, ...) {
  UseMethod("resample_idx")
}

#' @describeIn resample_idx Generate subsamples of rows of data frames.
#' @export
resample_idx.default <- function(data, samples, ...) {
  samples <- idx_list(samples)
  tibble(sample = map(samples, resample, data = data),
         .id = seq_len(samples))
}

#' @describeIn resample_idx Generate subsamples of groups of grouped data frames.
#' @export
resample_idx.grouped_df <- function(data, samples, ...) {
  samples <- idx_list(samples)
  idx <- group_indices_lst(data)
  f <- function(.g) {
    resample(data = data, idx = flatten_int(idx[.g]))
  }
  tibble(sample = map(samples, f), .id = seq_len(samples))
}

#' @describeIn resample_idx Generate subsamples of groups of grouped data frames.
#' @export
resample_idx.grouped_df <- function(data, samples, ...) {
  samples <- idx_list(samples)
  idx <- group_indices_lst(data)
  f <- function(.g) {
    resample(data = data, idx = flatten_int(idx[.g]))
  }
  tibble(sample = map(samples, f), .id = seq_len(samples))
}
