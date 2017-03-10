#' Create lazy resample object(s)
#'
#' @param data A data frame
#' @param idx An integer vector
#' @param check If \code{TRUE}, then run test that \code{idx} is consistent
#'   with
#' @return A \code{resample} object, a list with two elements: \code{data}
#'   and \code{idx}.
#' @seealso This function is a slight variant of \code{\link[modelr]{resample}}.
#'   The function \code{\link{resample_lst}} generates a list of resample objects.
#' @export
#' @examples
#' resample(mtcars, 1:10)
resample <- function(data, idx, check = TRUE) {
  assert_that(is.flag(check))
  if (check) {
    assert_that(is.data.frame(data))
    assert_that(is.numeric(idx))
    assert_that(all(!is.na(idx)))
    assert_that(all(idx >= 1) && all(idx <= nrow(data)))
  }
  structure(list(data = data, idx = as.integer(idx)), class = "resample")
}

#' Create a list of resample objects
#'
#' @param data A data frame
#' @param check If \code{TRUE}, check that \code{idx} are valid.
#' @param idx A list of integer vectors of indexes.
#' @seealso \code{\link{resample}} generates a single resample object.
#' @return A \code{list} of \code{\link{resample}} objects.
#' @export
#' @examples
#' resample_lst(mtcars, list(1:3, 4:6, 7:10))
resample_lst <- function(data, idx, check = TRUE) {
  map(idx, resample, data = data, check = check)
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
           flatten_int(map(objs, as.integer)), FALSE)
}
