#' Generate rolling windows
#'
#' Generate \code{\link[modelr]{resample}} objects for rolling windows.
#' For data frames, the windows are applied row-wise, for grouped data frames,
#' the windows are applied group-wise.
#'
#' @param data A data frame
#' @param by Windows are at every by-th point rather than every observation
#' @param width An integer specifying the window width.
#' @param ... Arguments passed to methods
#' @param partial If \code{FALSE}, then windows which extend outside the range
#'    of the data are not included. If \code{TRUE}, then those windows outside
#'    the range of the data are included, though they are truncated.
#' @param align Is the window aligned to the left, right, or center.
#'
#' @return A data frame with four columns
#' \describe{
#' \item{window}{A list column of \code{\link[modelr]{resample}} objects}
#' \item{.id}{integer: the index position of the window}
#' \item{.width}{integer: the width of the window}
#' \item{.align}{character: alignment of the window. "l" for left,
#' "c" for center, "r" for right}
#' }
#' @seealso The \code{\link[zoo]{rollapply}} function in \pkg{zoo}
#' @importFrom purrr map compact negate map_lgl
#' @importFrom tibble tibble
#' @importFrom stringr str_sub str_to_upper
#' @export
roll <- function(data, ...) {
  UseMethod("roll")
}

#' @rdname roll
#' @export
roll.data.frame <- function(data, width = 1L, by = 1L, from = 1L, to = nrow(data),
                            partial = FALSE,
                            align = c("center", "left", "right"),
                            offsets = NULL,
                            ...) {
  assert_that(is.flag(partial) || (is.number(partial) && partial >= 0))
  assert_that(is.number(width))
  assert_that(is.number(by))
  from <- max(from %||% 1L, 1L)
  assert_that(is.number(from))
  to <- min(to %||% nrow(data), nrow(data))
  assert_that(is.number(to))
  align <- match.arg(align)
  posn <- seq(from = from, to = to, by = by)
  windows <- compact(purrr::set_names(map(posn, data = data, width = width,
                                          partial = partial, align = align,
                                          offsets = offsets),
                                       posn))
  df <- tibble(
    window = unname(map(windows, resample, data = data)),
    .id = names(windows),
    .width = as.integer(width),
    .align = str_to_upper(str_sub(align, 1, 1))
  )
  df
}

#' @rdname roll
#' @export
roll.grouped_df <- function(data, width = 1L,
                            by = 1L, from = 1L, to = dplyr::n_groups(data),
                            partial = FALSE,
                            align = c("center", "left", "right"),
                            offsets = NULL,
                            ...) {
  align <- match.arg(align)
  posn <- seq(from = 1L, to = dplyr::n_groups(data), by = by)
  grp_windows <- compact(purrr::set_names(get_windows(posn, width = width,
                                                      partial = partial,
                                                      align = align,
                                                      offsets = offsets),
                                          posn))
  idx <- group_indices_lst(data)
  windows <- map(grp_windows, function(w) flatten_int(idx[w]))
  df <- tibble(
    window = unname(map(windows, resample, data = data)),
    .id = names(windows),
    .width = as.integer(width),
    .align = str_to_upper(str_sub(align, 1, 1))
  )
  df
}


get_window_idx <- function(i,
                           width = NULL, n = Inf, partial = TRUE,
                           align = "center", offsets = NULL) {
  offsets <- as.integer(offsets) %||% as.integer(switch(
      align,
      right = seq(to = 0L, length.out = width),
      center = seq(to = floor(width / 2), length.out = width),
      left = seq(from = 0L, length.out = width)
  ))
  idx <- i + offsets
  idx <- if (partial) {
    idx <- idx[idx >= 1L & idx <= n]
    if (length(idx) >= partial) as.integer(idx)
    else NULL
  } else {
    if (any(idx < 1L | idx > n)) NULL
    else as.integer(idx)
  }
}
