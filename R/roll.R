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
#' @param width The window width
#' @param align Is the window left-aligned, right-aligned, or centered?
#' @param offsets numeric. If not-\code{NULL}, then the offsets of the window
#'   indices relative to the reference index. If \code{NULL}, then the window
#'   offsets are generated from the \code{width} and \code{align} arguments.
#' @param indices,from,to,by The indices at which to generate windows. If
#'   \code{indices} is non-\code{NULL}, then it is used. Otherwise, the indices
#'   are generated from \code{seq(from, to, by)}.
#' @param ... Arguments passed to methods
#' @return A data frame
#' @export
roll <- function(data, ...) {
  UseMethod("roll")
}


#' @describeIn roll Data frame method. This rows are assumed to be ordered.
#' @export
roll.data.frame <- function(data,
                            width = 1L,
                            align = c("left", "right", "center"),
                            partial = TRUE,
                            indices = NULL,
                            from = 1L,
                            to = n,
                            by = 1L,
                            offsets = NULL, ...) {
  n <- nrow(data)
  assert_that(is.number(width) && width >= 1L)
  align <- match.arg(align)
  assert_that(is.flag(partial) ||
                (is.number(partial) && partial >= 1))
  assert_that(is.null(indices) || is.integer(indices))
  assert_that(is.number(from) && from >= 1L && from <= to)
  assert_that(is.number(to) && to >= from && to <= n)
  assert_that(is.number(by) && by >= 1)
  assert_that(is.null(offsets) || is.integer(offsets))
  res <- roll_(n, width = width, align = align, partial = partial,
               indices = indices, from = from, to = to, by = by,
               offsets = offsets)
  to_resample_df(res, data)
}


#' @describeIn roll Grouped data frame method. The groups are assumed to be ordered, and
#'   the cross validation works on groups rather than rows.
#' @export
roll.grouped_df <- function(data,
                  width = 1L,
                  align = c("left", "right", "center"),
                  partial = TRUE,
                  indices = NULL,
                  from = 1L,
                  to = n,
                  by = 1L,
                  offsets = NULL, ...) {
  grps <- group_indices_lst(data)
  n <- length(grps)
  assert_that(is.number(width) && width >= 1L)
  align <- match.arg(align)
  assert_that(is.flag(partial) ||
                (is.number(partial) && partial >= 1))
  assert_that(is.null(indices) || is.integer(indices))
  assert_that(is.number(from) && from >= 1L && from <= to)
  assert_that(is.number(to) && to >= from && to <= n)
  assert_that(is.number(by) && by >= 1)
  assert_that(is.null(offsets) || is.integer(offsets))
  f <- function(i) flatten_int(grps[i]) # nolint
  res <- mutate_(roll_(n, width = width, align = align, partial = partial,
                       indices = indices, from = from, to = to, by = by,
                       offsets = offsets),
                 sample = ~ map(sample, f))
  to_resample_df(res, data)
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
