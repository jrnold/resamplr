#' @importFrom rlang is_scalar_integerish
is_pos_scalar_integer <- function(x) {
  is_scalar_integerish(x) && x > 0
}

assertthat::on_failure(is_pos_scalar_integer) <- function(call, env) {
  paste0(deparse(call$x), " is not a positive length one integer vector.")
}

is_nonneg_integer_scalar <- function(x) {
  is_scalar_integerish(x) && x >= 0
}

assertthat::on_failure(is_nonneg_integer_scalar) <- function(call, env) {
  paste0(deparse(call$x), " is not a non-negative length one integer vector.")
}

#' @importFrom rlang is_scalar_double
is_scalar_proportion <- function(x, strict = TRUE) {
  is_scalar_double(x) && {
    if (strict) {
      x > 0 & x < 1
    } else {
      x >= 0 & x <= 1
    }
  }
}

assertthat::on_failure(is_scalar_proportion) <- function(call, env) {
  if (call$strict) {
    vals <- "between and including 0 and 1"
  } else {
    vals <- "between but excluding 0 and 1"
  }
  paste0(deparse(call$x), " is not a proportion (a length one numeric vector with values ", vals, ").")
}

#' Identify and extract (un)named elements.
#'
#'
#' @param x An object
#' @return \code{is_unnamed} and \code{is_named} return logical
#'   vectors the same length as \code{x}. \code{named} and \code{unnamed} return vectors that are the same type as \code{x}.
#'
#' @noRd
is_unnamed <- function(x) {
  rlang::names2(x) == ""
}

unnamed <- function(x) x[is_unnamed(x)]

is_named <- purrr::negate(is_unnamed)

named <- function(x) x[is_named(x)]

#' Extract elements
#'
#' This function is a convenience function for calling \code{[},
#' when the number of dimensions in unknown. The default \code{[}
#' expects there to be an argument for every dimension.
#'
#' @param x An object
#' @param ... For unnamed elements, indicies specifying elements to extract. These indices can be atomic vectors or lists of length two, where the first element is the dimension, and the second are the indicies in that dimension. Named arguments, like \code{drop}, are directly passed to \code{[}.
#' @noRd
#' @importFrom rlang dots_list missing_arg lang sym splice eval_bare names2
dextract <- function(x, ...) {
  dots <- dots_list(...)
  unnamed_dots <- unnamed(dots)
  d <- dim(x) %||% length(x)
  args <- rerun(length(d), missing_arg())
  for (i in seq_along(unnamed_dots)) {
    idx <- unnamed_dots[[i]]
    if (is.list(idx)) {
      if (length(idx) <= 1) {
        d <- i
        ii <- idx[[1]]
      } else {
        d <- idx[[1]]
        ii <- idx[[2]]
      }
    } else {
      d <- i
      ii <- idx
    }
    args[[d]] <- ii
  }
  cl <- lang(sym("["), x = quote(x), splice(args), splice(named(dots)))
  eval_bare(cl)
}