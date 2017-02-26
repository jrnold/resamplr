# adapted from boot:::ts.array
ts_idx <- function(n, n.sim, l, sim, endcorr) {
  endpt <- if (endcorr)
    n
  else n - l + 1
  if (sim == "geom") {
    len.tot <- 0
    lens <- NULL
    while (len.tot < n.sim) {
      temp <- 1 + rgeom(1, 1/l)
      temp <- pmin(temp, n.sim - len.tot)
      lens <- c(lens, temp)
      len.tot <- len.tot + temp
    }
    st <- sampl.int(endpt, length(lens), replace = TRUE)
  }
  else {
    nn <- ceiling(n.sim / l)
    lens <- c(rep(l, nn - 1), 1 + (n.sim - 1) %% l)
    st <- sample.int(endpt, nn, replace = TRUE)
  }
  map2_int(st, lens, function(s, l) {
    if (l > 1) seq(s, s + l - 1L)
    else integer()
  })
}

#' Generate a time-series bootstrap replicate
#'
#' @param data A data frame
#' @param len If \code{sim} is \code{"fixed"} then \code{len} is the fixed block
#'    length used in generating the replicate time series.
#'    If \code{sim} is \code{"geom"} then \code{len} is the mean of the
#'    geometric distribution used to generate the block lengths.
#'    \code{length} should be a positive integer less than the number of rows
#'    of \code{data}.
#' @param sim The type of simulation required to generate the replicate time series.
#'    The possible input values are "model" (model based resampling), "fixed" (block resampling with fixed block lengths of l), "geom" (block resampling with block lengths having a geometric distribution with mean l)
#' @param size Length of the simulated time series in the replicate.
#' @param A logical variable indicating whether end corrections are to be applied when sim is "fixed". When sim is "geom", endcorr is automatically set to TRUE; endcorr is not used when sim is "model" or "scramble".
#' @seealso \code{\link[boot]{tsboot}}
#' @export
resample_ts_bootstrap <- function(data,
                                  size = NULL,
                                  len = NULL,
                                  sim = c("fixed", "geom"),
                                  endcorr = FALSE) {
  assert_that(is.flag(endcorr))
  n <- nrow(data)
  size <- size %||% n
  assert_that(is.number(size))
  assert_that(size > 0)
  assert_that(is.number(len))
  assert_that(len > 0 & len < n)
  sim <- match.arg(sim)
  idx <- ts_idx(n, size, length, sim, endcorr)
  resample(data, idx)
}


ts_bootstrap <- function(data,
                         size = NULL,
                         len = NULL,
                         sim = c("fixed", "geom"),
                         endcorr = FALSE) {
  perm <- purrr::rerun(n, resample_permutation(data))
  df <- tibble::tibble(perm = perm)
  df[[id]] <- id(n)
  df
}
