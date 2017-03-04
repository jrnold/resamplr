#' @importFrom modelr resample
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map map_int map_lgl map_df flatten_int %||% transpose
NULL

#' Create a lazy resample object
#'
#' This is a slightly modified version of \code{\link[modelr]{resample}}.
#' It has more tests for the validity of the indexes, but also coerces
#' numeric \code{idx} to integers.
#'
#' @param data A data frame
#' @param idx An integer vector
#' @returns A \code{resample} object, a list with two elements: \code{data}
#'   and \code{idx}.
#' @export
resample <- function(data, idx) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (is.numeric(idx)) {
    idx <- as.integer(idx)
  }
  if (!is.integer(idx)) {
    stop("`idx` must be an integer vector.")
  } else if (any(is.na(idx))) {
    stop("All elements of `idx` must be non-missing integers.")
  } else if (any(idx < 1) || any(idx > nrow(data))) {
    stop("All elements of `idx` must be between 1 and `nrow(data)`.")
  }
  structure(list(data = data, idx = idx), class = "resample")
}

#' Create a list of resample objects
#'
#' @param data A data frame
#' @param idx A list of integer vectors of indexes.
#' @returns A \code{list} of \code{\link{resample}} objects.
#' @export
resample_lst <- function(data, idx) {
  map(idx, resample, data = data)
}

#' Is it a resample object?
#'
#' Checks whether an object is an \code{\link{resample}}
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

is_single_integer <- function(x) {
  is.integer(x) && (length(x) == 1)
}

is_natural_number <- function(x) {
  is_single_integer(x) && x >= 1
}


id <- function(n) {
  width <- nchar(n)
  sprintf(paste0("%0", width, "d"), seq_len(n))
}

group_indices_lst <- function(data) {
  g <- seq_len(dplyr::n_groups(data))
  split(dplyr::group_indices(data), g)
}

# Return either row numbers or group numbers
data_idx <- function(data) {
  UseMethod("data_idx")
}

data_idx.data.frame <- function(data) {
  seq_len(nrow(data))
}

data_idx.grouped_df <- function(data) {
  seq_len(dplyr::n_groups(data))
}
