#' @importFrom modelr resample
#' @importFrom tibble tibble
#' @importFrom purrr map map_int map_lgl map_df flatten_int %||% transpose
NULL

id <- function(n) {
  width <- nchar(n)
  sprintf(paste0("%0", width, "d"), seq_len(n))
}

#' Is it a resample object?
#'
#' Checks whether an object is an \code{\link[modelr]{resample}}
#' object.
#'
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
is.resample <- function(x) inherits(x, "resample")

#' @export
c.resample <- function(...) {
  objs <- list(...)
  if (length(objs) == 1) return(objs[[1]])
  if (!all(map_lgl(objs, is.resample))) {
    stop("All objects must inherit from class `resample`.",
         call. = FALSE)
  }
  identical_data <- function(x) identical(x[["data"]], objs[[1]][["data"]])
  if (!all(map_lgl(objs[-1], identical_data))) {
    stop("All resample objects must have identical data", call. = FALSE)
  }
  resample(objs[[1]][["data"]],
           flatten_int(map(objs, as.integer)))
}


# extract indexes of a numbered group from a grouped_df
get_group_indexes <- function(data, groups = NULL) {
  # indices are 0-indexed
  idx <- map(attr(data, "indices"), `+`, 1L)
  if (!is.null(groups)) idx <- idx[groups]
  idx
}

get_group_indexes_int <- function(data, groups = NULL) {
  flatten_int(get_group_indexes(data, groups = groups))
}

# split indexes by groups from a grouped_df
split_idx_by_group <- function(data, ids) {
  idx <- get_group_indexes_int(data, ids)
  list(as.integer(idx), as.integer(setdiff(seq_len(nrow(data)), idx)))
}

rpartition <- function(x, k) {
  n <- length(x)
  parts <- sample(rep(seq_len(k), length.out = n), n, replace = FALSE)
  split(x, parts)
}

group_ids <- function(data) seq_len(dplyr::n_groups(data))

# map_resample_df <- function(data, idx) {
#   tibble::as_tibble(transpose(map(idx, function(i, data) resample(data, i),
#                                   data = data)))
# }