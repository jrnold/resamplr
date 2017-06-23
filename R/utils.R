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
#' into an integer vector. Optionally apply a function to
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

# copied from modelr
#' @importFrom purrr reduce
reduce_common <- function(x, msg = "Objects must be identical", operator = identical) {
  reduce(x, function(.x, .y) {
    if (!operator(.x, .y)) {
      stop(msg, call. = FALSE)
    }
    .y
  })
}

# copied from modelr
big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

#' Append class to an object
#'
#' Add a class to an object
#' @param x object
#' @param newclass Character. New class(es) to add
#' @param after integer. location to add those classes.
#'
#' @noRd
append_class <- function(x, newclass, after = 0) {
  # use attr(x, "class") instead of class()
  # because class() would unnecessarily add primitive types
  class(x) <- append(attr(x, "class"), newclass, after = after)
  x
}

# coerce to a list of indices
idx_list <- function(x) {
  if (is.integer(x) || is.character(x) || is.numeric(x)) {
    # if a single valid vector - then use
    list(x)
  } else {
    # otherwise, coerce to a list
    rlang::as_list(x)
  }
}

# @param groups a list in which each element are the indexes in that group
# @param idx: a vector or list of indexes in which each element
#    is group index
#' @importFrom purrr as_vector
flatten_group_idx <- function(idx, groups) {
  as_vector(groups[idx])
}

flatten_group_idx_lst <- function(idx, groups) {
  map(idx_list(idx), flatten_group_idx, groups = groups)
}
