#' Create a lazy resample object
#'
#' This is a slightly modified version of \code{\link[modelr]{resample}}.
#' It has more tests for the validity of the indexes, but also coerces
#' numeric \code{idx} to integers.
#'
#' @param data A data frame
#' @param idx An integer vector
#' @return A \code{resample} object, a list with two elements: \code{data}
#'   and \code{idx}.
#' @export
#' @example inst/examples/resample.R
resample <- function(data, idx, check = FALSE) {
  assert_that(is.flag(check))
  if (check) {
    assert_that(is.data.frame(data))
    assert_that(is.numeric(idx))
    idx <- as.integer(idx)
    assert_that(all(!is.na(idx)))
    assert_that(all(idx >= 1) && all(idx < nrow(data)))
  }
  structure(list(data = data, idx = idx), class = "resample")
}

#' Create a list of resample objects
#'
#' @param data A data frame
#' @param idx A list of integer vectors of indexes.
#' @return A \code{list} of \code{\link{resample}} objects.
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
#' @importFrom purrr map_lgl
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
