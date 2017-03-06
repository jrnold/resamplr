#' Generate jackknife replicates
#'
#' Generate \code{n} jacknife replicates, each of which is \code{n - 1}
#' rows. For grouepd data frames, this generates \code{n_groups} replicates,
#' with each repliacate dropping all rows in a group.
#'
#' @param data A data frame
#' @return A data frame containing a single column, \code{jackknife},
#'   which is a list-columns containing \code{\link{resample}}
#' @param ... Arguments passed to methods
#' @export
jackknife <- function(data, ...) {
  UseMethod("jackknife")
}

#' @rdname jackknife
#' @export
jackknife.data.frame <- function(data, ...) {
  idx <- seq_len(nrow(data))
  tibble(
    jackknife = resample_lst(data, map(idx, setdiff, x = idx)),
    .id = id(nrow(data))
  )
}

#' @rdname jackknife
#' @export
jackknife.grouped_df <- function(data, ...) {
  idx <- group_indices_lst(data)
  grps <- seq_along(idx)
  tibble(
    jackknife = resample_lst(data, map(grps, function(g) {
      flatten_int(idx[setdiff(grps, g)])
    })),
    .id = id(length(idx))
  )
}
