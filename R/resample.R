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
#'
#' @param x An object.
#' @param ... Other arguments
#'
#' @details
#' The only case when \code{resample} is not lazy, is when \code{data} is itself
#' a \code{resample} object, the new \code{resample} object uses \code{data\$data},
#' and the \code{idx} indices of \code{data\$idx}.
#'
#' @export
resample <- function(data, idx, ...) {
  UseMethod("resample")
}

#' @export
resample.default <- function(data, idx, ...) {
  assert_that(is.integer(idx) || is.character(idx) || is.numeric(idx))
  if (is.numeric(idx)) idx <- as.integer(idx)
  structure(
    list(
      data = data,
      idx = idx
    ),
    class = "resample"
  )
}

#' @export
resample.resample <- function(data, idx, ...) {
  resample(data[["data"]], data[["idx"]][idx])
}

#' @export
resample.grouped_df <- function(data, idx, groups = TRUE, ...) {
  # expand group indexes to observation indexes
  if (groups) {
    indices <- attr(data, "indices")
    idx <- flatten_int(indices[idx])
  }
  resample.default(data, idx)
}

#' @importFrom tibble obj_sum
#' @export
obj_sum.resample <- function(x, ...) {
  paste0("resample of ", class(x$data)[1], " [", length(x$idx), "]")
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

#' @describeIn resample Return the indexes as a character vector.
#' @export
as.character.resample <- function(x, ...) {
  as.character(x$idx)
}

#' @describeIn resample Subset the data and return the results.
#'   The return value  should have same type as \code{x$data}.
#' @export
#' @importFrom dplyr collect
collect.resample <- function(x, ...) {
  resample_extract(x$data, x$idx, ...)
}

#' @export
dim.resample_df <- function(x, ...) {
  c(length(x$idx), ncol(x$data))
}

# nrow and ncol only make sense for tbl like objects

#' @export
nrow.resample <- function(x, ...) {
  if (length(dim(x$data)) == 2L) length(x$idx)
}

#' @export
ncol.resample <- function(x, ...) {
  if (length(dim(x$data)) == 2L) ncol(x$data)
}

#' Is it a resample object?
#'
#' Checks whether an object inherits from \code{\link[modelr]{resample}}.
#'
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
is_resample <- function(x) inherits(x, "resample")

#' @describeIn resample Concatenate \code{resample} and index objects into a
#'   single \code{resample} objects. The \code{data} element of the first
#'   object is used as the data for the returned obejct, with the \code{data}
#'   elements of other resample objects neither used nor checked.
#'   The elements of \code{...} must be \code{resample} objects, or
#'   \code{integer}, \code{numberic}, or \code{character} vectors, which are
#'   treated as indexes.
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
  resample(x[[1]][["data"]], idx = purrr::as_vector(map(x, f)))
}

#' Create a list of resample objects
#'
#' @param data A data frame
#' @param idx A list of integer vectors of indexes.
#' @param ... Arguments passed to \code{\link{resample}}.
#' @seealso \code{\link{resample}} generates a single resample object.
#' @return A \code{list} of \code{\link{resample}} objects.
#' @export
#' @examples
#' resample_lst(mtcars, list(1:3, 4:6, 7:10))
resample_lst <- function(data, idx, ...) {
  map(idx_list(idx), resample, data = data, ...)
}

# check if all elements in a list are resample objects
is_resample_lst <- function(x) all(map_lgl(x, is_resample))

#' @importFrom assertthat on_failure
assertthat::on_failure(is_resample_lst) <- function(call, env) {
  paste0("All elements of ", deparse(call$x), " must inherit from class `resample`")
}

#' Interal methods for \code{resampler} objects
#'
#' These are methods used to handle different types of \code{data} elements
#' in \code{resample} objects.
#' Instead of subclassing \code{resample} objects, it is easier to define
#' methods for the several key tasks needed.
#'
#' \code{resample_extract()} is called by \code{\link[resamplr]{collect}} to
#' extract the \code{idx} indices from the \code{data} element.
#'
#' \describe{
#' \item{resampler_idx_len}{The number of elements in \code{data}. This is needed by
#'   \code{bootstrap} and most resampling methods when sampling or partitioning
#'   indices.}
#' \item{resampler_extract}{extract the values
#' }
#'
#' @param x An object (the \code{data} element of a \code{resampler} object)
#' @param i Vector. Indices.
#' @param margin Integer. Margin of matrix or arrary to use for indices.
#' @param ... Additional arguments (currently unused)
#'
#' @return
#' \describe{
#' \item{resamplr_idx_len}{An integer of length 1.}
#' \item{resamplr_extract}{An object of the same class as \code{x}}
#' }
#' @name resampler-internal
NULL

#' @rdname resampler-internal
#' @export
resample_idx_len <- function(x, ...) {
  UseMethod("resample_idx_len")
}

#' @rdname resampler-internal
#' @export
resample_idx_len.default <- function(x, ...) length(x)

#' @rdname resampler-internal
#' @export
resample_idx_len.matrix <- function(x, ...) nrow(x)

#' @rdname resampler-internal
#' @export
resample_idx_len.array <- function(x, margin = 1L, ...) dim(x)[margin]

#' @rdname resampler-internal
#' @export
resample_idx_len.data.frame <- function(x, ...) nrow(x)

#' @rdname resampler-internal
#' @export
resample_idx_len.resample <- function(x, ...) length(x$idx)

#' @rdname resampler-internal
#' @export
resample_extract <- function(x, ...) {
  UseMethod("resample_extract")
}

#' @rdname resampler-internal
#' @export
resample_extract.default <- function(x, i, ...) x[i]

#' @rdname resampler-internal
#' @export
resample_extract.matrix <- function(x, i, margin = 1L, ...) {
  d <- rep(TRUE, 2)
  d[margin] <- i
  invoke(`[`, c(list(x = x), as.list(d), list(drop = FALSE)))
}

#' @rdname resampler-internal
#' @importFrom purrr invoke
#' @export
resample_extract.array <- function(x, i, margin = 1L, ...) {
  d <- dim(x)
  d[margin] <- i
  invoke(`[`, c(list(x = x), as.list(d), list(drop = FALSE)))
}


#' @rdname resampler-internal
#' @importFrom dplyr n_groups
#' @export
resample_idx_len.grouped_df <- function(x, groups = TRUE, ...) {
  # return number of groups or obs
  if (groups) {
    n_groups(x)
  } else {
    nrow(x)
  }
}
