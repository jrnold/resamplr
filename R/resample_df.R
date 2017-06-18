#' Generate subsamples from indices
#'
#' Generate subsamples from manually specified indices of observations in each
#' subsample.
#'
#' @param data An object
#' @param samples A list of integer vectors or an integer vector, with the indices in each subsample.
#' @param ... Arguments passed to methods
#' @templateVar numrows rows for each subsample and
#' @template return_resample_df
#' @example inst/examples/ex-resample_df.R
#' @export
resample_data <- function(data, ...) {
  UseMethod("resample_data")
}

# coerce to a list of indices
idx_list <- function(x) {
  if (is.integer(x) || is.character(x)) {
    # if a single valid vector - then use
    list(x)
  } else {
    # otherwise, coerce to a list
    rlang::as_list(x)
  }
}

#' @describeIn resample_df Generate subsamples of rows of data frames.
#' @export
resample_data.data.frame <- function(data, samples, ...) {
  samples <- idx_list(samples)
  tibble(sample = map(samples, resample, data = data),
         .id = seq_len(samples))
}

#' @describeIn resample_df Generate subsamples of groups of grouped data frames.
#' @export
resample_data.grouped_df <- function(data, samples, ...) {
  samples <- idx_list(samples)
  idx <- group_indices_lst(data)
  f <- function(.g) {
    resample(data = data, idx = flatten_int(idx[.g]))
  }
  tibble(sample = map(samples, f), .id = seq_len(samples))
}
