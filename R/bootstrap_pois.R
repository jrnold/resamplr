#' Generate Poisson Bootstrap Replicates
#'
#' Generates replicates from a Poisson bootstrap, which
#' samples each observation independently from a Poisson distribtuion
#' with \expr{lambda = 1} if all obs have equal probability.
#'
#' @param data a data frame
#' @param n Number of replicates to draw
#' @param size Expected size of each replicate sample
#' @param wt Observation weights. If \code{NULL}, all observations
#'   are weighted equally.
#' @export
pois_bootstrap <- function(data, n, size = NULL, wt = NULL) {
  strap <- purrr::rerun(n, resample_pois_bootstrap(data))
  df <- tibble::tibble(strap = strap)
  df[[id]] <- seq_len(nrow(df))
  df
}

#' @rdname pois_bootstrap
#' @export
resample_pois_bootstrap <- function(data, size = NULL, wt = NULL) {
  n <- nrow(data)
  size <- size %||% n
  if (is.null(wt)) {
    lambda <- 1
  } else {
    lambda <- sum2one(wt) * n
  }
  # adjust to get expected size
  if (size != n) {
    lambda <- lambda * (size / n)
  }
  idx <- flatten_dbl(map2(seq_len(n), rpois(n, lambda),
                          function(i, m) rep(i, m)))
  resample(data, idx)
}
