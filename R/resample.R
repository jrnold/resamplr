# --- From modelr

# copied from modelr
#' A "lazy" resample.
#'
#' Often you will resample a dataset hundreds or thousands of times. Storing
#' the complete resample each time would be very inefficient so this class
#' instead stores a "pointer" to the original dataset, and a vector of row
#' indexes. To turn this into a regular data frame, call \code{as.data.frame},
#' to extract the indices, use \code{as.integer}.
#'
#' @param data The data frame
#' @param idx A vector of integer indexes indicating which rows have
#'   been selected. These values should lie between 1 and \code{nrow(data)}
#'   but they are not checked by this function in the interests of performance.
#' @examples
#' r <- resample(mtcars, 1:10)
#' as.integer(r)
#' as.data.frame(r)
#' dim(r)
#' # check that the data part of r is identical to orig data
#' identical(r, mtcars)
#' @export
resample <- function(data, idx) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.integer(idx)) {
    stop("`idx` must be an integer vector.", call. = FALSE)
  }

  structure(
    list(
      data = data,
      idx = idx
    ),
    class = "resample"
  )
}

# copied from modelr
#' @export
print.resample <- function(x, ...) {
  n <- length(x$idx)
  if (n > 10) {
    id10 <- c(x$idx[1:10], "...")
  } else {
    id10 <- x$idx
  }

  cat("<", obj_sum.resample(x), "> ", paste(id10, collapse = ", "), "\n",
      sep = ""
  )
}

# copied from modelr
#' @export
as.integer.resample <- function(x, ...) {
  x$idx
}

# copied from modelr
#' @export
as.data.frame.resample <- function(x, ...) {
  x$data[x$idx, , drop = FALSE]
}

# copied from modelr
#' @export
dim.resample <- function(x, ...) {
  c(length(x$idx), ncol(x$data))
}

# copied from modelr
#' @importFrom tibble obj_sum
#' @method obj_sum resample
#' @export
obj_sum.resample <- function(x, ...) {
  paste0("resample [", big_mark(nrow(x)), " x ", big_mark(ncol(x)), "]")
}

# ----

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
  map(idx, resample, data = data)
}

#' Is it a resample object?
#'
#' Checks whether an object inherits from \code{\link[modelr]{resample}}.
#'
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
is_resample <- function(x) inherits(x, "resample")

is_resample_lst <- function(x) all(map_lgl(x, is_resample))

#' @importFrom assertthat on_failure
assertthat::on_failure(is_resample_lst) <- function(call, env) {
  paste0("All elements of ", deparse(call$x), " must inherit from class `resample`")
}


#' @export
#' @importFrom purrr map_lgl is_integer compact
c.resample <- function(...) {
  x <- list(...)
  if (length(x) == 1) {
    return(x[[1]])
  }
  if (!all(map_lgl(x, function(.x) is_resample(.x) || is_integer(.x)))) {
    stop("Can only concatenate `resample` and `integer` objects",
         call. = FALSE)
  }
  reduce_common(compact(map(x, "data")),
                msg = "All elements of `x` must have identical `data`")
  # This can work even if earlier checks are failed
  # as long as all objects have as.integer methods.
  # However, it seems safer to be stricter and force the user to ensure
  # that all objects are resample or integer
  resample(x[[1]][["data"]], flatten_int(map(x, as.integer)))
}
