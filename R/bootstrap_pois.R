#' @export
bootstrap_pois <- function(data, ...) {
  UseMethod("bootstrap_pois")
}

#' @export
bootstrap_pois.default <- function(data, R = 1L, lambda = 1, ...) {
  out <- bootstrap_pois_(resample_idx_len(data), R = R, lambda = 1)
  out[["sample"]] <- resample_lst(data, out[["sample"]], ...)
  out
}

#' @export
bootstrap_pois.grouped_df <- function(data, R = 1L, lambda = 1, ...) {
  out <- bootstrap_pois_(resample_idx_len(data, groups = TRUE),
                         R = R, lambda = 1)
  out[["sample"]] <- resample_lst(data, out[["sample"]], groups = TRUE)
  out
}


#' @importFrom stats rpois
bootstrap_pois_1 <- function(n, lambda = 1) {
  flatten_int(map2(seq_len(n), rpois(n, lambda), rep.int))
}

bootstrap_pois_ <- function(n, R = 1L, lambda = 1, ...) {
  tibble(sample = rerun(R, bootstrap_pois(n, lambda = lambda, ...)))
}
