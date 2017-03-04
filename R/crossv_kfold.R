#' Generate cross-validated k-fold test-training pairs
#'
#' @param data A data frame
#' @param k Number of folds
#' @param ... Passed to methods
#' @export
crossv_kfold <- function(data, k, ...) {
  f <- function(i) resample_holdout(data, test = i)
  df <- as_tibble(transpose(map(split_kfold(data_idx(data), k), f)))
  df[[id]] <- id(k)
  df
}
