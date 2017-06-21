#' Generate subsamples from indices
#'
#' Generate subsamples from manually specified indices of observations in each
#' subsample.
#'
#' @param data An object
#' @param idx A list of integer vectors or an integer vector, with the indices in each subsample.
#' @param ... Arguments passed to methods
#' @templateVar numrows rows for each subsample and
#' @template return_resample
#' @example inst/examples/ex-resample.R
#' @export
resample_idx <- function(data, idx, ...) {
  UseMethod("resample_idx")
}

#' @export
resample_idx.default <- function(data, idx, ...) {
  idx <- idx_list(idx)
  tibble(sample = resample_lst(data, idx), .id = seq_along(idx))
}

#' @export
resample_idx.grouped_df <- function(data, idx, ...) {
  idx <- idx_list(idx)
  gidx <- group_indices_lst(data)
  idxs <- map(idx, function(.g) flatten_int(gidx[.g]))
  tibble(sample = resample_lst(data, idxs), .id = seq_along(idx))
}

#' @export
resample_idx.resample <- function(data, idx, ...) {
  idx <- idx_list(idx)
  tibble(
    sample = resample_lst(data, idx),
    .id = seq_along(idx)
  )
}
