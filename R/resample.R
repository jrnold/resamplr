#' A "lazy" resample object
#'
#' Often you will resample a dataset many times, as in cross-validation or bootstrapping.
#' Copying and storing the complete resample each time is costly in both memory and time.
#' This object instead stores the original data as a \code{\link[rlang]{quosure}} and indexes, and only computes the sample object when needed.
#'
#' @section Usage:
#' \preformatted{smpl <- Resample$new(data, idx, fun = NULL)
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
#' \code{print(smpl)} or \code{smpl$print()} shows some information about the \code{Resample} object on the screen.
#'
#' @name resample
#' @examples
#' smpl <- Resample$new(mtcars, 1:5)
#' smpl$idx
#' smpl$data
#' smpl$sample
#' smpl$quo
NULL

#' @rdname resample
#' @importFrom rlang enquo eval_tidy
#' @importFrom R6 R6Class
#' @importFrom purrr %||%
#' @importFrom rlang as_quosure UQ
#' @export
Resample <- R6::R6Class("resample",
  public = list(
    quo = NULL,
    idx = NULL,
    extractor = NULL,
    initialize = function(data, idx, extractor = NULL) {
      # TODO: really should check and reject if not quosure
      self$quo <- as_quosure(data, env = env)
      self$idx <- as_index(idx)
      self$extractor <- rlang::as_function(extractor %||% extract_sample)
      self
    },
    get_sample = function(..., .cache = TRUE) {
      self$extractor(eval_tidy(UQ(self$quo)), self$idx, ...)
    }
  ),
  active = list(
    sample = function() {
      self$get_sample()
    },
    data = function() eval_tidy(UQ(self$quo))
  )
  # TODO: is is worth caching samples?
  # tradeoff of time and memory - maybe an object level switch?
  # private = list(cached_sample = NULL)
)

# So ... why R6? I was considering the following:
#
# - quosure -- but then the user has to run `tidy_eval` ... which seems suboptimal. Also to get indexes and other information need to provide methods and store that information as attributes.
# - function -- Nice in that user only needs to do `x()` to evaluate
#   the sample. But then the information about the original quosure and indexes is lost, and needs to be stored as attributes,
# - list -- could store the fuction, quosure, and indexes as elements. But it cannot introspect itself, so duplication is required.
# - environment -- Basically, this is the R6 solution. The active bindings are the killer feature. It looks like data to the user, but it is lazily computed, like we want.

#' @rdname resample
#' @export
resample <- function(x, idx, extractor = NULL) {
  Resample$new(as_quosure(x), idx, extractor = extractor)
}

#' @rdname resample
#' @importFrom purrr map
#' @export
resample_lst <- function(x, idx, extractor = NULL) {
  x <- as_quosure(x)
  map(idx, Resample$new, data = x, extractor = extractor)
}

#' @importFrom tibble tibble
#' @export
resample_idx <- function(x, idx, extractor = NULL) {
  tibble(sample = resample_lst(x, idx, extractor = extractor))
}

#' @export
print.resample <- function(x, ...) {
  n <- length(x$idx)
  if (n > 10) {
    id10 <- c(x$idx[1:10], "...")
  }
  else {
    id10 <- x$idx
  }
  cat("<resample>\n")
  cat("data: ", deparse(x$quo), "\n")
  cat("idx [", length(x$idx), "]: ",
      paste(id10, collapse = ", "), "\n", sep = "")
}

#' @importFrom tibble obj_sum
#' @export
obj_sum.resample <- function(x, ...) {
  paste0(obj_sum(x), " [", length(x$idx), "]")
}

#' @importFrom tibble type_sum
#' @export
type_sum.resample <- function(x, ...) {
  "R6: resample"
}

#' @export
as.data.frame.resample <- function(x, ...) {
  as.data.frame(x$sample)
}

extract_sample <- function(x, idx, ...) {
  UseMethod("extract_sample")
}

extract_sample.default <- function(x, idx, ...) {
  x[idx]
}

extract_sample.matrix <- function(x, idx, rows = TRUE, ...) {
  if (rows) {
    x[idx, , drop = FALSE]
  } else {
    x[ , idx, drop = FALSE]
  }
}

#' @importFrom rlang missing_arg splice eval_bare prepend lang splice
#'
#' @importFrom purrr rerun
extract_sample.array <- function(x, idx, margin = 1L, ...) {
  # need all this to work around the requirement that
  # [.array has args for every dimension
  args <- rerun(length(dim(x)), missing_arg())
  args[[margin]] <- idx
  args <- prepend(args, list(x = quote(x)))
  eval(lang("[", splice(args)))
}

extract_sample.data.frame <- function(x, idx, rows = TRUE, ...) {
  if (rows) {
    x[idx, , drop = FALSE]
  } else {
    x[ , idx, drop = FALSE]
  }
}

extract_sample.quosure <- function(x, ...) {
  extract_sample(eval_tidy(x), ...)
}

#' Ensure that a vector is
#'
#' Only integers are character vectors should be used for subsetting
#' in resample objects, so convert a variety of objects to ensure
#' that they are valid.

#'
#' @param x The object to be converted.
#' @return An integer or character vector
#' @noRd
as_index <- function(x) {
  UseMethod("as_index")
}

as_index.default <- as.character

as_index.logical <- function(x) {
  if (!is.null(names(x))) {
    names(x)[x]
  } else {
    which(x)
  }
}

as_index.integer <- identity

as_index.numeric <- as.integer

#' Generate the number of elemenets
#'
#' This method provides number of elements from which to sample.
#' This number is needed by the resampling functions. This is better
#' than explicit enumeration of indexes, since resampling does not
#' require the explicit enumeration.
#'
#' @param x An object
#' @return A scalar integer
#' @noRd
idx_len <- function(x, ...) {
  UseMethod("idx_len")
}

idx_len.default <- length

idx_len.array <- function(x, margin = 1L) {
  dim(x)[margin]
}

idx_len.matrix <- function(x, rows = TRUE) {
  if (rows) nrow(x) else ncol(x)
}

idx_len.data.frame <- function(x, rows = TRUE) {
  if (rows) nrow(x) else ncol(x)
}

idx_len.quosure <- function(x, ...) {
  idx_len(eval_tidy(x), ...)
}

#' Enumerate indexes
get_idx <- function(x, ...) {
  UseMethod("get_idx")
}

# Get index
get_idx.default <- function(x) {
  seq_len(idx_len(x))
}

get_idx.quosure <- function(x, ...) {
  get_idx(eval_tidy(x), ...)
}