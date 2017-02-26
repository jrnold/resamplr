#' Generate n balanced bootstrap replicates
#'
#' @param data A data frame
#' @param n number of replicates
#' @param strata strata
#' @param clusters clusters
balanced_bootstrap <- function(data, n,
                               strata = FALSE,
                               clusters = FALSE) {
  if (strata) {
    idx <- group_idx(data)
    f <- function(x, B) {
      as.data.frame(matrix(sample(rep(x, times = B)), ncol = n))
    }
    map(unname(as.list(map_df(idx, f, B = B))),
        resample, data = data)
  } else {
    map(as.list(as.data.frame(matrix(sample(rep(idx, times = n)), ncol = B))),
        resample, data = data)
  }
}
