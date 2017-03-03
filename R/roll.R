#' Generate a window
#'
#' Generate a \code{\link[modelr]{resample}} object for a window.
#'
#'
#' @param data A data frame
#' @param idx An integer index
#' @param width The width of the window
#' @param align Alignment of the window relative to \code{idx}
#' @param partial If \code{TRUE}, and the window is not fully inside the data,
#'   then a truncated window is returned. Otherwise, \code{NULL} is returned.
#' @return A \code{resample} object or \code{NULL}, if the window falls outside
#'   the data.
#' @export
resample_window <- function(data, idx, width, partial = TRUE,
                            align = c("left", "right", "center")) {
  align <- match.arg(align)
  indexes <- get_window_idx(idx, width, nrow(data), partial = partial,
                            align = align)
  if (!is.null(indexes)) {
    resample(data, as.integer(indexes))
  } else {
    NULL
  }
}


#' Generate rolling windows
#'
#' Generate \code{\link[modelr]{resample}} objects for rolling windows.
#'
#' @inheritParams resample_window
#' @param by Start windows at every by-th point rather than every point.
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
roll <- function(data, width, by = 1L,
                 partial = FALSE,
                 align = c("center", "left", "right")) {
  align <- match.arg(align)
  posn <- seq(from = 1L, to = nrow(data), by = by)
  window <- map(posn, resample_window, data = data, width = width,
                partial = partial, align = align)
  posn <- posn[map_lgl(window, negate(is.null))]
  df <- tibble(
    window = compact(window),
    .id = posn,
    .width = as.integer(width),
    .align = str_to_upper(str_sub(align, 1, 1))
  )
  df
}

get_window_idx <- function(i, width, n, partial, align) {
  offset <- as.integer(switch(
    align,
    right = seq(to = 0L, length.out = width),
    center = seq(to = floor(width / 2), length.out = width),
    left = seq(from = 0L, length.out = width)
  ))
  idx <- i + offset
  idx <- if (partial) {
    idx <- idx[idx >= 1L & idx <= n]
    if (length(idx)) idx
    else NULL
  } else {
    if (any(idx < 1L | idx > n)) NULL
    else idx
  }
}
