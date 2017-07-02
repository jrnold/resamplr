#' Ensure a vector is a valid index
#'
#' Only integers are character vectors should be used for subsetting
#' in lazy_sample objects, so convert a variety of objects to ensure that they are valid indexes.
#'
#' @param x The object to be converted
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
