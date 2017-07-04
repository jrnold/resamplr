#' Generate the number of elements to sample from
#'
#' This method provides number of elements from which to sample.
#' This number is needed by the resampling functions.
#' This is better than explicit enumeration of indexes, since resampling does not require the explicit enumeration.
#'
#' @param x An object
#' @param ... Arguments passed to other methods
#' @return A scalar integer
#' @example inst/examples/ex-idx_len.R
#' @export
idx_len <- function(x, ...) {
  UseMethod("idx_len")
}

#' @describeIn idx_len The default is to use the \code{length} of the object.
#' @export
idx_len.default <- function(x, margin = 1L, ...) {
  d <- dim(x)
  if (is.null(d)) {
    length(x)
  } else {
    d[margin]
  }
}

#' @describeIn idx_len For arrays, specify the margin for the number of elements. The default is the first margin.
#' @param margin Integer scalar; the array margin to use
#' @export
idx_len.array <- function(x, margin = 1L, ...) {
  dim(x)[margin[1L]]
}

#' @describeIn idx_len For matrices, there are two dimensions, so specify whether the number of rows or columns are to be used.
#' @param rows Logical scalar. If \code{TRUE}, use return number of rows, else the number of columns.
#' @export
idx_len.matrix <- function(x, rows = TRUE, ...) {
  if (rows) nrow(x) else ncol(x)
}

#' @describeIn idx_len For data frames, there are two dimensions, so specify whether the number of rows or columns are to be used.
#' @export
idx_len.data.frame <- function(x, rows = TRUE, ...) {
  if (rows) nrow(x) else ncol(x)
}

#' @describeIn idx_len For quosures, the expression is evaluated and then \code{idx_len} is called on the result.
#' @export
idx_len.quosure <- function(x, ...) {
  idx_len(eval_tidy(x), ...)
}

spell_check <- function() {
  c("Biometrika",
    "Breiman",
    "Burman",
    "Canty",
    "Davison",
    "Efron",
    "eleents",
    "Hinkley",
    "idx",
    "indices",
    "integerish",
    "len",
    "Olshen",
    "quosure",
    "quosureish",
    "quosures")
}