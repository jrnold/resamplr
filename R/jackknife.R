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
jackknife <- function(data, id = ".id") {
  idx <- seq_len(nrow(data))
  jack <- map(idx, function(i) resample_jackknife(data, i))
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
#' @param i The row index number to exclude
#' @return A \code{\link[modelr]{resample}} object.
#' @export
resample_jackknife <- function(data, i) {
  idx <- seq_len(nrow(data))
  resample(data, setdiff(idx, i))
}
