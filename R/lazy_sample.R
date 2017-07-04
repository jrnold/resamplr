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
#' smpl$extractor
#' smpl$draw_sample()
#' smpl$sample
#' smpl$data
#'
#' print(smpl)
#' as.data.frame(smpl)
#'
#' }
#'
#' @section Arguments:
#' \describe{
#' \item{data}{An unevaluated expression that will be captured as a quosure.}
#' \item{idx}{A character or integer vector. If not these, it will be coerced to an integer (numeric) or character vector (all other classes). Since \code{data} is not evaluated when the object is created, the validity of these indexes is not checked.}
#' \item{extractor}{A function with two arguments (data, index) which is used to extract the sample when it is computed. This is coerced to a function via \code{\link[rlang]{as_function}}, meaning that a formula like \code{~ .x[.y]} can be used. If \code{NULL}, then \code{\link{extract_sample}} is used.}
#' }
#'
#' @section Details:
#'
#' \code{$idx} returns the indexes in the sample.
#'
#' \code{$quo} returns a \code{\link[rlang]{quosure}} that returns the data to be sampled from when evaluated with \code{\link[rlang]{eval_tidy}}.
#'
#' \code{$data} returns the data to be sampled from.
#'
#' \code{$sample} or \code{$draw_sample()} or \code{draw_sample(smpl)} returns the sample. \code{$sample} is is active binding and thus the sample is lazily calculated.
#'
#' \code{print(smpl)} or \code{smpl$print()} shows some information about the \code{LazySample} object on the screen.
#'
#' \code{as.data.frame(smpl)} returns the sample and coerces it to a data frame. This is a convenience function since many modeling functions, e.g. \code{lm}, call \code{as.data.frame} on the \code{data} argument.
#'
#' @name LazySample
#' @seealso \code{\link{lazy_sample}} is a convenience function to create lazy sample objects, \code{\link{lazy_sample_lst}} creates a list of lazy samples objects, and \code{\link{lazy_sample_idx}} creates a data frame with a list column of lazy sample objects.
#' @example inst/examples/ex-LazySample.R
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
      self$quo <- as_quosure(expr)
      self$idx <- as_index(idx)
      self$extractor <- if (!is.null(extractor)) {
        rlang::as_function(extractor)
      } else {
        extract_sample
      }
      self
    },
    draw_sample = function(...) {
      self[["extractor"]](eval_tidy(self[["quo"]]), self[["idx"]], ...)
    }
  ),
  active = list(
    sample = function() {
      self[["draw_sample"]]()
    },
    data = function() eval_tidy(self[["quo"]])
  )
)

#' @export
as.data.frame.lazy_sample <- function(x, ...) {
  as.data.frame(x[["sample"]])
}

#' @export
print.lazy_sample <- function(x, ...) {
  n <- length(x[["idx"]])
  if (n > 10) {
    id10 <- c(x[["idx"]][1:10], "...")
  }
  else {
    id10 <- x[["idx"]]
  }
  cat("<lazy_sample>\n")
  cat("data: ", deparse(x[["quo"]]), "\n")
  cat("idx [", length(x[["idx"]]), "]: ",
      paste(id10, collapse = ", "), "\n", sep = "")
}

#' @importFrom tibble obj_sum
#' @export
obj_sum.lazy_sample <- function(x, ...) {
  paste0("lazy_sample [", length(x[["idx"]]), "]")
}

#' @importFrom tibble type_sum
#' @export
type_sum.lazy_sample <- function(x, ...) {
  "lazy_sample"
}

# So ... why R6? I was considering the following:
#
# - quosure -- but then the user has to run `tidy_eval` ... which seems suboptimal. Also to get indexes and other information need to provide methods and store that information as attributes.
# - function -- Nice in that user only needs to do `x()` to evaluate
#   the sample. But then the information about the original quosure and indexes is lost, and needs to be stored as attributes,
# - list -- could store the fuction, quosure, and indexes as elements. But it cannot introspect itself, so duplication is required.
# - environment -- Basically, this is the R6 solution. The active bindings are the killer feature. It looks like data to the user, but it is lazily computed, like we want.

#' Create lazy sample objects
#'
#' The function \code{lazy_sample} is a convenience function to create
#' \code{LazySample} objects. The function \code{lazy_sample_lst} creates a list
#' of lazy sample objects with the same expression, but multiple indexes.
#' The function \code{lazy_sample_idx} creates a data frame with a list-column of \code{LazySample} objects.
#' @param expr An expression with the data.
#' @param idx A list of integer or character vector with the indexes in each sample.
#' @template param-extractor
#' @return
#' \describe{
#' \item{lazy_sample}{A \code{LazySample} object.}
#' \item{lazy_sample_lst}{A list of \code{LazySample} objects with length equal to the length of \code{idx}.}
#' \item{lazy_sample_idx}{A data frame with one column and rows equal to the length of \code{idx}.
#' The single column is named \code{sample} and is a list of \code{LazySample} objects.}
#' }
#' @seealso \code{\link{LazySample}} for detailed information on the \code{lazy_sample} class.
#' @example inst/examples/ex-lazy_sample.R
#' @export
lazy_sample <- function(expr, idx, extractor = NULL) {
  LazySample$new(enquo(expr), idx, extractor = extractor)
}

#' @rdname lazy_sample
#' @importFrom purrr map
#' @export
lazy_sample_lst <- function(expr, idx, extractor = NULL) {
  map(idx, LazySample$new, expr = enquo(expr), extractor = extractor)
}

#' @rdname lazy_sample
#' @importFrom tibble tibble
#' @export
lazy_sample_idx <- function(expr, idx, extractor = NULL) {
  tibble(sample = lazy_sample_lst(as_quosure(expr), idx, extractor = extractor))
}
