#' Generate time-series bootstraps replicates
#'
#' @details
#' Bergmeir et al. (2016) describe a moving block bootstrap (MBB) method.
#' Suppose a time series of size \eqn{n} and a chosen block size of size \eqn{l}.
#' Randomly sample \eqn{\lfloor n / l \rfloor + 2}{floor(n / l) + 2} blocks of
#' size \eqn{l} and concatenate them into a single series
#' Then randomly discard 0 to \eqn{l - 1} values from the start of the series,
#' and truncate the series to length \eqn{n}. The later correction ensures that
#' every value from the original series can be placed anywhere in the bootstrapped
#' series.
#'
#' @param data A data frame
#' @param size Block length size
#' @param k Number of bootstrap replicates
#' @param ... Arguments passed to methods
#'
#' @references
#' \itemize{
#' \item{Kunsch, H.R. (1989) The jackknife and the bootstrap for general stationary observations. \emph{Annals of Statistics}}
#' \item{Lahiri, S., 2003. \emph{Resampling Methods for Dependent Data}. Springer}
#' \item{Bergmeir, Christoph, Rob J. Hyndman, and Jose M. Benitz. 2016. "Bagging Exponential Smoothing Methods using STL Decomposition and Box-Cox Transformation." \emph{International Journal of Forecasting} \href{http://dx.doi.org/10.1016/j.ijforecast.2015.07.002}{doi:10.1016/j.ijforecast.2015.07.002}.}
#' }
#' @export
tsbootstrap <- function(data, ...) {
  UseMethod("tsbootstrap")
}

#' @describeIn tsbootstrap Method for data frames, sampling the rows of the data frame.
#' @export
tsbootstrap.data.frame <- function(data, k = 1L, size = 1L, ...) {
  assert_that(is.number(k) && k >= 1)
  assert_that(is.number(size) && size >= 1L && size <= nrow(data))
  to_resample_df(tsbootstrap_(nrow(data), k = k, size = size), data)
}

#' @describeIn tsbootstrap Method for grouped data frames.
#'   This samples groups instead of rows.
#' @export
tsbootstrap.grouped_df <- function(data, k = 1L, size = 1L, ...) {
  idx <- group_indices_lst(data)
  f <- function(i) flatten_int(idx[i])
  res <- mutate_(tsbootstrap_(length(idx), k = k, size = size),
                 sample = ~ map(sample, f))
  to_resample_df(res, data)
}

# Fixed Moving Block Bootstrap from boot::tsboot
# .tsboot_mbb <- function(n, m, size = 1, endcorr = TRUE) {
#   mod <- function(i, n) {
#     if (endcorr) 1 + (i - 1) %% n
#     else i
#   }
#   endpt <- if (endcorr) {
#     n
#   } else {
#     n - size + 1
#   }
#   nn <- ceiling(m / size)
#   lens <- c(rep(size, nn - 1), 1 + (m - 1) %% size)
#   st <- sample.int(endpt, nn, replace = TRUE)
#   purrr::map2_int(st, lens, function(s, sz) {
#     if (sz > 1) mod(seq(s, s + sz - 1L), n)
#     else integer()
#   })
# }

# Fixed Moving Block Bootstrap from boot::tsboot
# .tsboot_geom <- function(n, m, size = 1) {
#   mod <- function(i, n) 1 + (i - 1) %% n
#   endpt <- n - size + 1
#   len_tot <- 0
#   lens <- NULL
#   while (len_tot < m) {
#     temp <- 1 + stats::rgeom(1, 1 / size)
#     temp <- pmin(temp, m - len_tot)
#     lens <- c(lens, temp)
#     len_tot <- len_tot + temp
#   }
#   st <- sample.int(endpt, length(lens), replace = TRUE)
#   purrr::map2_int(st, lens, function(s, sz) {
#     if (sz > 1) mod(seq.int(s, s + sz - 1L), n)
#     else integer()
#   })
# }

# MBB from forecast package
# https://github.com/robjhyndman/forecast/blob/a57c996e809d17ed29f5a9e74d344b3ef4df3ed7/R/bootstrap.R#L44
mbb <- function(n, size = 1L) {
  n_blocks <- floor(n / size) + 2L
  res <- flatten_int(map(sample.int(n - size + 1L, n_blocks),
                         function(i) i + seq.int(size) - 1L))
  # discard 0 to size - 1 values
  start <- sample.int(size, 1L)
  end <- start + n - 1L
  res[start:end]
}

tsbootstrap_ <- function(n, k = 1L, size = 1) {
  tibble(sample = purrr::rerun(k, mbb(n, size)), .id = seq_len(k))
}
