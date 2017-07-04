#' Extract samples from objects
#'
#' The generic function \code{\link{extract_sample}} is the function that
#' extracts the elements specified by \code{idx} from the the data in a
#' \code{lazy_sample} object.
#'
#' It is largely for internal use in order to abstract the details for
#' extracting elements from data frames, vectors, matrices, arrays, etc.
#' However, it is exported so that the user can add methods for new objects
#' or change the default behavior. The \code{extractor} field in \code{lazy_sample}
#' objects and the \code{extractor} argument in many functions will override
#' this function.
#'
#' @family lazy sample internal functions
#'
#' @param x An object
#' @param idx An integer or character vector representing index values
#' @param ... Arguments used by methods.
#' @example inst/examples/ex-extract_sample.R
#' @export
extract_sample <- function(x, idx, ...) {
  UseMethod("extract_sample")
}

#' @describeIn extract_sample The default method is equivalent to \code{x[idx]} when there are no dimensions in \code{x}. When there are dimensions in \code{x}, then \code{idx} refers to the \code{margin} dimension.
#'
#' @param drop A logical scalar. If \code{TRUE}, then coerce to the lowest possible dimension. See \code{\link{drop}}.
#' @param margin An integerish scalar. The margin (dimension) to which \code{idx} refer. For example, \code{extract(x, idx, 1)} is like \code{x[idx, ]}, \code{extract(x, idx, 2)} is like \code{x[ , idx, ...]}, and so on.
#' @export
extract_sample.default <- function(x, idx, margin = 1L, ...) {
  dextract(x, list(margin, idx), ...)
}

#' @describeIn extract_sample The default method for matrices extracts rows by default and does not drop dimensions.
#' @param rows A logical scalar. If \code{TRUE}, then extract rows, else extract columns.
#' @export
extract_sample.matrix <- function(x, idx, rows = TRUE, drop = FALSE, ...) {
  if (rows) {
    x[idx, , drop = FALSE]
  } else {
    x[ , idx, drop = FALSE]
  }
}

#' @describeIn extract_sample The data frame method extracts rows by default, and does not drop dimensions.
#' @export
extract_sample.data.frame <- function(x, idx, rows = TRUE, drop = FALSE, ...) {
  if (rows) {
    x[idx, , drop = drop]
  } else {
    x[ , idx, drop = drop]
  }
}

#' @describeIn extract_sample For quosures, the method will evaluate the expression and then call the next method on the result.
extract_sample.quosure <- function(x, ...) {
  extract_sample(eval_tidy(x), ...)
}
