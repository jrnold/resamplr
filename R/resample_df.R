#' Generate subsamples from indices
#'
#' Generate subsamples from manually specified indices of observations in each
#' subsample.
#'
#' @param data A data frame
#' @param samples A list of integer vectors or an integer vector, with the indices in each subsample.
#' @param ... Arguments passed to methods
#' @templateVar numrows rows for each subsample and
#' @template return_resample_df
#' @example inst/examples/ex-resample_df.R
#' @export
resample_df <- function(data, ...) {
  UseMethod("resample_df")
}

#' @describeIn resample_df Generate subsamples of rows of data frames.
#' @export
resample_df.data.frame <- function(data, samples, ...) {
  if (is.integer(samples)) samples <- list(samples)
  assert_that(is.list(samples) && all(map_lgl(samples, is.integer)))
  to_resample_df(resample_df_(samples), data)
}

#' @describeIn resample_df Generate subsamples of groups of grouped data frames.
#' @export
resample_df.grouped_df <- function(data, samples, ...) {
  if (is.integer(samples)) samples <- list(samples)
  assert_that(is.list(samples) && all(map_lgl(samples, is.integer)))
  idx <- group_indices_lst(data)
  f <- function(i, idx) flatten_int(idx[i])
  res <- resample_df_(samples)
  res[["sample"]] <- map(res[["sample"]], f, idx = idx)
  to_resample_df(res, data)
}

resample_df_ <- function(samples) {
  tibble(sample = samples, .id = seq_along(samples))
}
