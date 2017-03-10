#' @importFrom modelr resample
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map flatten_int transpose %||%
#' @importFrom assertthat assert_that is.number is.flag
NULL

#' Extract group indexes
#'
#' @param data A data frame
#' @return A list of integer vectors; each element of the
#'   list is a group, each vector contains the data frame
#'   indexes of that group.
#' @noRd
group_indices_lst <- function(data) {
  g <- dplyr::group_indices(data)
  split(seq_along(g), g)
}


# convert resample data frame with index list column to one with resample
# list column.
to_resample_df <- function(x, .data) {
  x[["sample"]] <- resample_lst(.data, x[["sample"]], check = FALSE)
  x
}

to_crossv_df <- function(x, data) {
  x[["train"]] <- resample_lst(data, x[["train"]], check = FALSE)
  x[["test"]] <- resample_lst(data, x[["test"]], check = FALSE)
  x
}
