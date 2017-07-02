#' Generate the number of elements to sample from
#'
#' This method provides number of elements from which to sample.
#' This number is needed by the resampling functions.
#' This is better than explicit enumeration of indexes, since resampling does not require the explicit enumeration.
#'
#' @param x An object
#' @param ... Arguments passed to other methods
#' @return A scalar integer
#' @export
idx_len <- function(x, ...) {
  UseMethod("idx_len")
}

#' @describeIn idx_len The default is to use the \code{length} of the object.
idx_len.default <- length

#' @describeIn idx_len For arrays, specify the margin for the number of elements. The default is the first margin.
#' @param margin Integer scalar; the array margin to use
idx_len.array <- function(x, margin = 1L, ...) {
  dim(x)[margin[1L]]
}

#' @describeIn idx_len For matrices, there are two dimensions, so specify whether the number of rows or columns are to be used.
#' @param rows Logical scalar. If \code{TRUE}, use return number of rows, else the number of columns.
idx_len.matrix <- function(x, rows = TRUE) {
  if (rows) nrow(x) else ncol(x)
}

#' @describeIn idx_len For data frames, there are two dimensions, so specify whether the number of rows or columns are to be used.
idx_len.data.frame <- function(x, rows = TRUE) {
  if (rows) nrow(x) else ncol(x)
}

#' @describeIn idx_len For quosures, the expression is evaluated and then `idx_len` is called on the result.
idx_len.quosure <- function(x, ...) {
  idx_len(eval_tidy(x), ...)
}
