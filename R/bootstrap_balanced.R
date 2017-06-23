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
#' @seealso The \pkg{boot} function \code{\link[boot]{boot}} which is the
#'   canonical R bootstrap implementation.
#' @references
#' \itemize{
#' \item{Gleason, John 1988. "Algorithms for Balanced Bootstrap Simulations." \emph{The American Statistician}
#' \href{https://dx.doi.org/10.2307/2685134}{doi:10.2307/2685134}.}
#' \item{Davison, A.C., Hinkley, D. V., and Schechtman, E. 1986.
#' "Efficient Bootstrap Simulation." \emph{Biometrika}.}
#' \item{Angelo Canty and Brian Ripley (2016). boot: Bootstrap R (S-Plus) Functions. R package version 1.3-18.}
#' }
#' @export
bootstrap_balanced <- function(data, ...) {
  UseMethod("balanced_bootstrap")
}

#' @export
bootstrap_balanced.default <- function(data, R = 1L, ...) {
  out <- bootstrap_balanced_(resample_idx_len(data), R = R)
  out[["sample"]] <- resample_lst(data, out[["resample"]])
  out
}

#' @export
bootstrap_balanced.grouped_df <- function(data, R = 1L, ...) {
  stop("balanced_bootstrap.grouped_df not implemented yet")
}

bootstrap_balanced_ <- function(n, R = 1L) {
  tibble(sample = partition(rep(seq_len(n), R), R))
}
