#' A lazy sample object
#'
#' Often you will resample a dataset many times, as in cross-validation or bootstrapping.
#' Copying and storing each complete sample is costly in both memory and time.
#' This object instead stores the original data as a \code{\link[rlang]{quosure}} and indexes, and only computes the sample object when needed.
#'
#' @section Usage:
#' \preformatted{smpl <- LazySample$new(data, idx, fun = NULL)
#'
#' smpl$idx
#' smpl$data
#' smpl$sample
#' smpl$extractor
#'
#' print(smpl)
#'
#' }
#'
#' @section Arguments:
#' \describe{
#' \item{data}{An unevaluated expression that will be captured as a quosure.}
#' \item{idx}{A character or integer vector. If not these, it will be coerced to an integer (numeric) or character vector (all other classes). Since \code{data} is not evaluated when the object is created, the validity of these indexes is not checked.}
#' \item{extractor}{A function with two arguments (data, index) which is used to extract the sample when it is computed. This is coerced to a function via \code{\link[rlang]{as_function}}, a formula like \code{~ .x[.y]} is acceptable.If \code{NULL}, then the \code{extract_sample} method is used.
#' }
#' }
#'
#' @section Details:
#'
#' \code{$sample}{Returns the sample. This is not calculated until needed, so it is lazy}
#'
#' \code{print(smpl)} or \code{smpl$print()} shows some information about the \code{LazySample} object on the screen.
#'
#' @name LazySample
#' @examples
#' # New lazy sample object
#' smpl <- LazySample$new(mtcars, 1:5)
#' # Get the indexes
#' smpl$idx
#' # Get the expression containing the data
#' smpl$quo
#' # Get the sample - it is not computed until called
#' smpl$sample
#' # Get the original data
#' smpl$data
NULL

#' @importFrom rlang enquo eval_tidy
#' @importFrom R6 R6Class
#' @importFrom purrr %||%
#' @importFrom rlang as_quosure UQ
#' @export
#' @noRd
LazySample <- R6::R6Class("lazy_sample",
  class = TRUE,
  # saves a little space
  cloneable = FALSE,
  public = list(
    quo = NULL,
    idx = NULL,
    extractor = NULL,
    initialize = function(expr, idx, extractor = NULL) {
      # TODO: really should check and reject if not quosure
      self$quo <- as_quosure(expr, env = env)
      self$idx <- as_index(idx)
      self$extractor <- rlang::as_function(extractor %||% extract_sample)
      self
    },
    get_sample = function(...) {
      self$extractor(eval_tidy(UQ(self$quo)), self$idx, ...)
    }
  ),
  active = list(
    sample = function() {
      self$get_sample()
    },
    data = function() eval_tidy(UQ(self$quo))
  )
)

# So ... why R6? I was considering the following:
#
# - quosure -- but then the user has to run `tidy_eval` ... which seems suboptimal. Also to get indexes and other information need to provide methods and store that information as attributes.
# - function -- Nice in that user only needs to do `x()` to evaluate
#   the sample. But then the information about the original quosure and indexes is lost, and needs to be stored as attributes,
# - list -- could store the fuction, quosure, and indexes as elements. But it cannot introspect itself, so duplication is required.
# - environment -- Basically, this is the R6 solution. The active bindings are the killer feature. It looks like data to the user, but it is lazily computed, like we want.

#' Create lazy sample objects
#'
#' The function \code{lazy_sample} is a convience function to create
#' \code{LazySample} objects. The function \code{lazy_sample_lst} creates a list
#' of lazy sample objects with the same expression, but multiple indexes.
#' The function \code{lazy_sample_idx} creates a data frame with a list-column of \code{LazySample} objects.
#' @param expr A quosure
#' @param idx A list of integer or character vector with the indexes in
#'    each sample.
#' @param extractor See \code{\link{lazy_sample}}.
#' @return
#' \describe{
#' \item{lazy_sample}{A \code{LazySample} object.}
#' \item{lazy_sample_lst}{A list of \code{LazySample} objects with length equal to the length of \code{idx}.}
#' \item{lazy_sample_idx}{A data frame with one column and rows equal to the length of \code{idx}.
#' The single column is named \code{sample} and is a list of \code{LazySample} objects.}
#' }
#' @export
lazy_sample <- function(expr, idx, extractor = NULL) {
  LazySample$new(as_quosure(expr), idx, extractor = extractor)
}

#' @rdname lazy_sample
#' @importFrom purrr map
#' @export
lazy_sample_lst <- function(expr, idx, extractor = NULL) {
  expr <- as_quosure(expr)
  map(idx, LazySample$new, expr = expr, extractor = extractor)
}

#' @rdname lazy_sample
#' @importFrom tibble tibble
#' @export
lazy_sample_idx <- function(expr, idx, extractor = NULL) {
  tibble(sample = lazy_sample_lst(as_quosure(expr), idx, extractor = extractor))
}

#' @export
print.lazy_sample <- function(x, ...) {
  n <- length(x$idx)
  if (n > 10) {
    id10 <- c(x$idx[1:10], "...")
  }
  else {
    id10 <- x$idx
  }
  cat("<lazy_sample>\n")
  cat("data: ", deparse(x$quo), "\n")
  cat("idx [", length(x$idx), "]: ",
      paste(id10, collapse = ", "), "\n", sep = "")
}

#' @importFrom tibble obj_sum
#' @export
obj_sum.lazy_sample <- function(x, ...) {
  paste0(obj_sum(x), " [", length(x$idx), "]")
}

#' @importFrom tibble type_sum
#' @export
type_sum.lazy_sample <- function(x, ...) {
  "R6: lazy_sample"
}

#' @export
as.data.frame.lazy_sample <- function(x, ...) {
  as.data.frame(x$sample)
}
