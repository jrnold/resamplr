#' Generate balanced bootstrap replicates
#'
#' A balanced bootstrap generates \code{k} replicates in which each element
#' appears \code{k} times.
#'
#' @param data A data frame
#' @param k Number of replicates
#' @param ... Arguments passed to methods
#' @param stratify If \code{TRUE}, resample within groups, otherwise,
#'   resample by groups instead of rows.
#' @return A data frame with \code{k} rows and columns:
#' \describe{
#' \item{\code{sample}}{A list of \code{\link{resample}} objects with
#' with the bootstrap replicates.}
#' \item{\code{.id}}{An integer vector with replicate ids}
#' }
#'
#' @references
#' \itemize{
#' \item{Gleason, John R. 1988. "Algorithms for Balanced Bootstrap Simulations." \emph{The American Statistician}
#' \href{https://dx.doi.org/10.2307/2685134}{doi:10.2307/2685134}.}
#' \item{Davison, A.C., Hinkley, D.V., and Schechtman, E. 1986.
#' "Efficient Bootstrap Simulation." \emph{Biometrika}.}
#' }
#'
#' @export
balanced_bootstrap <- function(data, ...) {
  UseMethod("balanced_bootstrap")
}

#' @describeIn balanced_bootstrap Balanced bootstrap of rows in a data frame.
#' @export
balanced_bootstrap.data.frame <- function(data, k = 1L, ...) {
  assert_that(is.number(k) && k >= 1L)
  res <- balanced_bootstrap_(nrow(data), k = k)
  to_resample_df(res, data)
}

#' @describeIn balanced_bootstrap Balanced bootstrap either by group, or
#'   within groups (if \code{stratify = TRUE}).
#' @export
balanced_bootstrap.grouped_df <- function(data, k = 1L, stratify = FALSE, ...) {
  assert_that(is.number(k) && k >= 1L)
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(data)
  if (stratify) {
    f <- function(g, .group) {
      mutate_(balanced_bootstrap_(length(g), k),
              sample = ~ map(sample, function(i) g[i]),
              .group = ~ rerun(k, rep(.group, length(g))),
              .id = ~ seq_len(k))
    }
    res <- summarise_(group_by_(map2_df(idx, seq_along(idx), f), ".id"),
                      sample = ~ list(flatten_int(sample)),
                      .group = ~ list(flatten_int(.group)))
  } else {
    res <- mutate_(balanced_bootstrap_(length(idx), k = k),
                   .group = ~ map(sample, function(i) {
                      rep(seq_along(i), map_int(idx[i], length))
                   }),
                   sample = ~ map(sample, function(i) flatten_int(idx[i])))
  }
  to_resample_df(res, data)[, c("sample", ".group", ".id")]
}

balanced_bootstrap_ <- function(n, k = 1L) {
  tibble(sample = partition(rep(seq_len(n), k), k), .id = seq_len(k))
}
