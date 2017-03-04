#' Generate jackknife replicates
#'
#' Generate \code{n} jacknife replicates, each of which is \code{n - 1}
#' rows.
#'
#' @param data A data frame
#' @param id Name of variable that gives each model a unique integer id.
#' @return A data frame containing a single column, \code{jackknife},
#'   which is a list-columns containing \code{\link{resample}}
#' @export
jackknife <- function(data, id = ".id", ...) {
  UseMethod("jackknife")
}

#' @describeIn jackknife Generate a list of \code{nrow(data)} resample objects,
#'   with each replicate dropping a row.
#' @export
jackknife.data.frame <- function(data, id = ".id", ...) {
  idx <- seq_len(nrow(data))
  jack <- map(idx, function(i) resample_jackknife(data, i))
  df <- tibble(jackknife = jack)
  df[[id]] <- id(nrow(df))
  df
}

#' @describeIn jackknife Generate a list of \code{n_groups(data)} resample
#'    objects, with each replicate dropping rows in a group.
#' @export
jackknife.data.frame <- function(data, id = ".id", ...) {
  jack <- map(seq_len(dplyr::n_groups(data)),
              function(i) resample_jackknife(data, i))
  df <- tibble(jackknife = jack)
  df[[id]] <- id(nrow(df))
  df
}

#' Generate a jacknkife replicate
#'
#' Generate a jacknife replicate, which includes all rows of \code{data},
#' but a given row \code{i}.
#'
#' @param data A data frame
#' @param i The row (in \code{data.frame}) or group (in \code{grouped_df})
#'   number to exclude.
#' @return A \code{\link[modelr]{resample}} object.
#' @export
resample_jackknife <- function(data, i) {
  UseMethod("resample_jackknife")
}

#' @export
resample_jackknife.data.frame <- function(data, i, ...) {
  idx <- setdiff(seq_len(nrow(data)), i)
  resample(data, idx)
}

#' @export
resample_jackknife.grouped_df <- function(data, i, ...) {
  idx <- get_group_indexes_int(data, setdiff(group_ids(data), i))
  resample(data, idx)
}
