#' Generate subsamples from indices
#'
#' Generate subsamples from manually specified indices of observations in each
#' subsample.
#'
#' @param data A data frame
#' @param samples A list of integer vectors, with the indices in each subsample.
#' @param ... Arguments passed to methods
#' @templateVar numrows rows for each subsample and
#' @template return_resample_df
#' @export
resample_df <- function(data, ...) {
  UseMethod("resample_df")
}

#' @describeIn resample_df Generate subsamples of rows of data frames.
#' @export
resample_df.data.frame <- function(data, samples, ...) {
  assert_that(is.list(samples) && all(map_lgl(samples, is.integer)))
  to_resample_df(resample_df_(nrow(data), samples), data)
}

#' @describeIn resample_df Generate subsamples of groups of grouped data frames.
#' @export
resample_df.grouped_df <- function(data, samples, ...) {
  assert_that(is.list(samples) && all(map_lgl(samples, is.integer)))
  idx <- group_indices_lst(data)
  res <- mutate_(resample_df_(length(idx), samples),
                 sample = ~ map(sample, function(i) idx[i]))
  to_resample_df(res, data)
}

resample_df_ <- function(n, samples) {
  tibble(sample = samples, .id = seq_len(samples))
}