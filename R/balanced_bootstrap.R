#' Generate balanced bootstrap replicates
#'
#' A balanced bootstrap generates \code{R} replicates in which each element
#' appears \code{R} times.
#'
#' @param data A data frame
#' @param R Number of replicates
#' @param ... Arguments passed to methods
#' @param stratify If \code{TRUE}, resample within groups, otherwise,
#'   resample by groups instead of rows.
#' @templateVar numrows \code{R} rows and
#' @seealso The \pkg{boot} function \code{\link[boot]{boot}} which is the
#'   canonical R bootstrap implementation.
#' @template return_resample_df
#' @references
#' \itemize{
#' \item{Gleason, John 1988. "Algorithms for Balanced Bootstrap Simulations." \emph{The American Statistician}
#' \href{https://dx.doi.org/10.2307/2685134}{doi:10.2307/2685134}.}
#' \item{Davison, A.C., Hinkley, D. V., and Schechtman, E. 1986.
#' "Efficient Bootstrap Simulation." \emph{Biometrika}.}
#' \item{Angelo Canty and Brian Ripley (2016). boot: Bootstrap R (S-Plus) Functions. R package version 1.3-18.}
#' }
#' @export
#' @example inst/examples/ex-balanced_bootstrap.R
balanced_bootstrap <- function(data, ...) {
  UseMethod("balanced_bootstrap")
}

#' @describeIn balanced_bootstrap Balanced bootstrap of rows in a data frame.
#' @export
balanced_bootstrap.data.frame <- function(data, R = 1L, ...) {
  assert_that(is.number(R) && R >= 1L)
  res <- balanced_bootstrap_(nrow(data), R = R)
  to_resample_df(res, data)
}

#' @describeIn balanced_bootstrap Balanced bootstrap either by group, or
#'   within groups (if \code{stratify = TRUE}).
#' @export
balanced_bootstrap.grouped_df <- function(data, R = 1L, stratify = FALSE, ...) {
  assert_that(is.number(R) && R >= 1L)
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(data)
  if (stratify) {
    f <- function(g, .group) {
      mutate_(balanced_bootstrap_(length(g), R),
              sample = ~ map(sample, function(i) g[i]),
              .group = ~ rerun(R, rep(.group, length(g))),
              .id = ~ seq_len(R))
    }
    res <- summarise_(group_by_(map2_df(idx, seq_along(idx), f), ".id"),
                      sample = ~ list(flatten_int(sample)),
                      .group = ~ list(flatten_int(.group)))
  } else {
    res <- mutate_(balanced_bootstrap_(length(idx), R = R),
                   .group = ~ map(sample, function(i) {
                      rep(seq_along(i), map_int(idx[i], length))
                   }),
                   sample = ~ map(sample, function(i) flatten_int(idx[i])))
  }
  to_resample_df(res, data)[, c("sample", ".group", ".id")]
}

balanced_bootstrap_ <- function(n, R = 1L) {
  tibble(sample = partition(rep(seq_len(n), R), R), .id = seq_len(R))
}
