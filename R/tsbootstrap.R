#' Generate time-series bootstrap replicates
#'
#' Generate \code{R} bootstrap replicates for time series.
#' The replicate time series can be generated using either fixed or random
#' block lengths.
#'
#' @details If \code{type} is \code{"fixed"} then each replicate time series is found
#' by taking blocks of length \code{size}, from the original time series and
#' putting them end-to-end until a new series of length \code{m} is created.
#' When \code{type} is \code{"geom"}, a similar approach is taken except
#' that the block lengths are sampled from a geometric distribution with mean
#' \code{size}.
#'
#' @param data A data frame
#' @param R Number of bootstrap replicates
#' @param size If sim is \code{"fixed"} then \code{size} is the fixed block
#'    length used in generating the replicate time series. If sim is
#'    \code{"geom"} then \code{size} is the mean of the geometric distribution
#'    used to generate the block lengths. \code{size} should be a positive
#'    integer less than the number of rows (\code{data.frame}) or groups
#'    \code{grouped_df} in \code{data}.
#' @param type Type of simulation used to generate the replicate time series.
#'    The possible input values are "fixed" (block resampling with fixed block
#'    lengths of \code{size}), "geom" (block resampling with block lengths
#'    having a geometric distribution with mean \code{size}).
#' @param m Lengths of time-series replicates
#' @param ... Arguments passed to methods
#' @seealso The \pkg{boot} function \code{\link[boot]{tsboot}}, from which this
#'    function is derived.
#' @templateVar numrows \code{R} rows and
#' @template return_resample_df
#' @references
#' \itemize{
#' \item{Kunsch, H.R. (1989) The jackknife and the bootstrap for general stationary observations. \emph{Annals of Statistics}}
#' \item{Davison, A.C. and Hinkley, D.V. (1997) \emph{Bootstrap Methods and Their Application}. Cambridge University Press.}
#' \item{Politis, D.N. and Romano, J.P. (1994) "The stationary bootstrap. Journal of the American Statistical Association."}
#' \item{Angelo Canty and Brian Ripley (2016). boot: Bootstrap R (S-Plus) Functions. R package version 1.3-18.}
#' }
#' @export
tsbootstrap <- function(data, ...) {
  UseMethod("tsbootstrap")
}

TSBOOT_TYPES <- c("fixed", "geom")

#' @describeIn tsbootstrap Generate time series replicates using blocks of
#'   rows in the data frame.
#' @export
tsbootstrap.data.frame <- function(data, R = 1L, size = 1L, m = nrow(data),
                                   type = "fixed", ...) {
  assert_that(is.number(R) && R >= 1)
  assert_that(is.number(size) && size >= 1L && size <= nrow(data))
  assert_that(type %in% TSBOOT_TYPES)
  to_resample_df(tsbootstrap_(nrow(data), R = R, size = size, type = type),
                 data)
}

#' @describeIn tsbootstrap Generate time series replicates using blocks of
#'   groups in the grouped data frame.
#' @export
tsbootstrap.grouped_df <- function(data, R = 1L, size = 1L, type = "fixed",
                                   m = dplyr::n_groups(data), ...) {
  assert_that(is.number(R) && R >= 1)
  assert_that(is.number(size) && size >= 1L && size <= nrow(data))
  assert_that(type %in% TSBOOT_TYPES)
  idx <- group_indices_lst(data)
  f <- function(i) flatten_int(idx[i])  # nolint
  res <- mutate_(tsbootstrap_(length(idx), R = R, size = size, type = type),
                 sample = ~ map(sample, f))
  to_resample_df(res, data)
}

# Adapted from boot:::make.ends
mod <- function(i, n, endcorr) {
  if (endcorr) 1 + (i - 1) %% n
  else i
}

# Fixed Moving Block Bootstrap from boot::tsboot and boot:::ts.array
# Fixed Moving Block Bootstrap from boot::tsboot
.tsboot_mbb <- function(n, m = n, size = 1L, endcorr = TRUE) {
  endpt <- if (endcorr) n else n - size + 1L
  nn <- ceiling(m / size)
  lens <- c(rep(size, nn - 1L), 1L + (m - 1L) %% size)
  st <- sample.int(endpt, nn, replace = TRUE)
  flatten_int(purrr::map2(st, lens, function(s, l) {
    if (l >= 1) as.integer(mod(seq(s, s + l - 1L), n, endcorr))
    else integer()
  }))
}

# Fixed Moving Block Bootstrap from boot::tsboot and boot:::ts.array
# adapted from
# use ... to ignore endcorr
.tsboot_geom <- function(n, m = n, size = 1L, ...) {
  endpt <- n - size + 1L
  # worst case scenario is to draw m. So take m draws from
  # geom and truncate
  lens <- 1L + stats::rgeom(m, 1L / size)
  len_tot <- cumsum(lens)
  # truncate to minimum length >= m
  lens <- lens[seq_len(purrr::detect_index(len_tot, ~ .x >= m))]
  st <- sample.int(endpt, length(lens), replace = TRUE)
  f <- function(s, l) {
    if (l >= 1) as.integer(mod(seq.int(s, s + l - 1L), n, TRUE))
    else integer()
  }
  utils::head(flatten_int(purrr::map2(st, lens, f)), m)
}

tsbootstrap_ <- function(n, R = 1L, m = n, size = 1L, type = "fixed",
                         endcorr = TRUE) {
  f <- switch(type,
              geom = .tsboot_geom,
              fixed = .tsboot_mbb,
              stop("type = ", type, " is not recognized.", call. = FALSE))
  tibble(sample = purrr::rerun(R, f(n, m = m, size = size, endcorr = endcorr)),
         .id = seq_len(R))
}
