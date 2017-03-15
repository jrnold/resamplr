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
  unname(split(seq_along(g), g))
}

#' Replace group indices with row indices
#'
#' Replace group indices with row indices and concatenate
#' into an integer vector. Optionall apply a function to
#' each group prior to concatenation (allowing for resampling)
#' on each group.
#'
#' @param x An list of integer vector of group indices
#' @param group_inx A list of integer vectors of the indices in
#'   in each group.
#' @param f A function to apply to each group prior to concatenation.
#' @return A list of integer vectors of row indices
#' @noRd
replace_groups <- function(x, group_idx, f = identity, ...) {
  map(x, function(g) flatten_int(map(group_idx[g], f, ...)))
}

# convert resample data frame with index list column to one with resample
# list column.
to_resample_df <- function(x, .data) {
  x[["sample"]] <- resample_lst(.data, x[["sample"]], check = FALSE)
  x
}

to_crossv_df <- function(x, .data) {
  x[["train"]] <- resample_lst(.data, x[["train"]], check = FALSE)
  x[["test"]] <- resample_lst(.data, x[["test"]], check = FALSE)
  x
}
