#' Generate rolling windows
#'
#' Generate resample objects of rolling windows over elements of a data frame.
#'
#' @param data A data frame
#' @param partial logical or numeric. If \code{FALSE} all indexes of a rolling
#'   window must be within the rows (or groups) of \code{data}. If \code{TRUE},
#'   then the subset of indexes that are in range are used.
#'   A numeric argument to \code{partial} can be used to determine the minimal
#'   window size for partial windows.
#' @param width The window width.
#' @param align Is the window left-aligned, right-aligned, or centered
#'   relative to the reference index of the window.
#' @param offsets An integer vector or \code{NULL}. If non-\code{NULL},
#'   then it is the offsets of elements in the window relative to the
#'   reference index of the window; \code{0} is the reference index, negative
#'   numbers are indices before the index, while positive numbers are after the
#'   index. If \code{NULL}, the offsets are generated from the \code{width}
#'   and \code{align} arguments.
#' @param indices,from,to,by The indices at which to generate windows. If
#'   \code{indices} is non-\code{NULL}, then it is used. Otherwise, the indices
#'   are generated from \code{seq(from, to, by)}.
#' @param ... Arguments passed to methods
#' @export
roll <- function(data, ...) {
  UseMethod("roll")
}

roll_ <- function(n,
                  width = 1L,
                  align = c("left", "right", "center"),
                  partial = TRUE,
                  indices = NULL,
                  from = 1L,
                  to = n,
                  by = 1L,
                  offsets = NULL) {
  assert_that(is.number(width) && width >= 1L)
  align <- match.arg(align)
  assert_that(is.flag(partial) ||
                (is.number(partial) && partial >= 1))
  assert_that(is.null(indices) || is.integer(indices))
  assert_that(is.number(from) && from >= 1L && from <= to)
  assert_that(is.number(to) && to >= from && to <= n)
  assert_that(is.number(by) && by >= 1)
  assert_that(is.null(offsets) || is.integer(offsets))

  offsets <- offsets %||% switch(
    align,
    right = seq(to = 0L, length.out = width),
    center = seq(to = floor(width / 2), length.out = width),
    left = seq(from = 0L, length.out = width)
  )
  offsets <- as.integer(offsets)
  f <- function(i) {
    window <- i + offsets
    inrange <- window >= 1 & window <= n
    if (all(inrange) || (partial && (sum(inrange) >= partial))) {
      tibble(sample = list(window[inrange]), .id = i)
    } else {
      NULL
    }
  }
  indices <- indices %||% seq(from, to, by)
  map_df(indices, f)
}
