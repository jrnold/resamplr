#' A "lazy" resample object
#'
#' Often you will resample a dataset many times, as in cross-validation or bootstrapping.
#' Copying and storing the complete resample each time is both time-consuming and memory intensive.
#' However, all that is needed for the resample
#' The \code{resample}
#' instead stores a "pointer" to the original dataset, and a vector of row
#' indexes.
#'
#' @param data An object. This can be any object for which a \code{[} method is defined.
#' @param idx A vector of integer indexes. For efficiency, this function does not check whether these indices are valid.
#' @param ... Other arguments
#' @examples
#' r <- resample(mtcars, 1:10)
#' as.integer(r)
#' as.data.frame(r)
#' dim(r)
#' # check that the data part of r is identical to orig data
#' identical(r, mtcars)
#' @export
resample <- function(data, idx, ...) {
  UseMethod("resample")
}

#' @export
resample.default <- function(data, idx, ...) {
  assert_that(is.integer(idx))
  structure(
    list(
      data = data,
      idx = idx
    ),
    class = "resample"
  )
}

#' @export
resample.data.frame <- function(data, idx, ...) {
  append_class(resample.default(data, idx), "resample_df")
}

#' @export
print.resample <- function(x, ...) {
  n <- length(x$idx)
  if (n > 10) {
    id10 <- c(x$idx[1:10], "...")
  } else {
    id10 <- x$idx
  }
  cat("<", obj_sum(x), "> ", paste(id10, collapse = ", "), "\n",
      sep = ""
  )
}

#' @describeIn resample Return the indexes as an integer vector.
#' @export
as.integer.resample <- function(x, ...) {
  as.integer(x$idx)
}

# TODO: should I let this work with character indices?
# or be opinionated and allow only integer indices.
# I could convert character to integer in the resample stage?

#' @describeIn resmple Return the indexes as a character vector.
#' @export
as.character.resample <- function(x, ...) {
  as.character(x$idx)
}

#' @describeIn resample Subset the data and return the results.
#'   The return value  should have same type as \code{x$data}.
#' @export
collect.resample <- function(x, ...) {
  if (dim(x$data) > 1) {
    x$data[x$idx, ...]
  } else {
    x$data[x$idx]
  }
}

#' @importFrom dplyr collect
#' @export
collect.resample_df <- function(x, ...) {
  x$data[x$idx, ..., drop = FALSE]
}

# copied from modelr
#' @export
as.data.frame.resample_df <- collect.resample_df

#' @describeIn resample The length of the indexes.
#' @export
length.resample <- function(x, ...) {
  length(x$idx)
}

#' @describeIn resample_df Length of the \code{idx} element, whhen \code{data} element is a data frame.
#' @export
dim.resample_df <- function(x, ...) {
  c(length(x$idx), ncol(x$data))
}

# nrow and ncol only make sense for tbl like objects
#' @describeIn resample Length of the \code{idx} element, whhen \code{data} element is a data frame.
#' @export
nrow.resample_df <- function(x, ...) length(x$idx)

#' @describeIn resample Number of columns in the \code{data} element.
#' @export
ncol.resample_df <- function(x, ...) ncol(x$data)

#' @importFrom tibble obj_sum
#' @method obj_sum resample
#' @export
obj_sum.resample <- function(x, ...) {
  paste0("resample ", length(x), " from ", obj_sum(x$data), "]")
}

#' Create a list of resample objects
#'
#' @param data A data frame
#' @param idx A list of integer vectors of indexes.
#' @seealso \code{\link{resample}} generates a single resample object.
#' @return A \code{list} of \code{\link[modelr]{resample}} objects.
#' @export
#' @examples
#' resample_lst(mtcars, list(1:3, 4:6, 7:10))
resample_lst <- function(data, idx) {
  map(idx_list(idx), resample, data = data)
}

#' Is it a resample object?
#'
#' Checks whether an object inherits from \code{\link[modelr]{resample}}.
#'
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
is_resample <- function(x) inherits(x, "resample")

#' @describeIn resample_df Concatenate \code{resample} and index objects into a single \code{resample} objects. The \code{data} element of the first object is used as the data for the returned obejct, with the \code{data} elements of other
#' resample objects neither used nor checked.
#' The elements of \code{...} must be \code{resample} objects, or \code{integer},
#' \code{numberic}, or \code{character} vectors, which are treated as indexes.
#' @export
#' @importFrom purrr map_lgl is_integer compact
c.resample <- function(...) {
  x <- list(...)
  if (length(x) == 1) {
    return(x[[1]])
  }
  # if a resample object, get indices, otherwise treat as
  # indices
  f <- function(.x) {
    if (is_resample(.x)) {
      .x[["idx"]]
    } else if (is.character(.x) || is.integer(.x)) {
      .x
    } else if (is.numeric(.x)) {
      as.integer(.x)
    } else {
      NULL
    }
  }
  # don't use flatten_int to still allow case of character indexes
  resample(x[[1]][["data"]], idx = purrr::flatten(map(x, f)))
}

# check if all elements in a list are resample objects
is_resample_lst <- function(x) all(map_lgl(x, is_resample))

#' @importFrom assertthat on_failure
assertthat::on_failure(is_resample_lst) <- function(call, env) {
  paste0("All elements of ", deparse(call$x), " must inherit from class `resample`")
}