#' Generate cross-validated k-fold test-training pairs
#'
#' @inheritParams modelr::crossv_kfold
#' @param ... Passed to methods
#' @export
crossv_kfold <- function(data, k, ...) {
  if (!is.numeric(k) || length(k) != 1) {
    stop("`k` must be a single integer.", call. = FALSE)
  }
  UseMethod("crossv_kfold")
}

#'@export
crossv_kfold.data.frame <- function(data, k = 5, id = ".id", ...) {
  f <- function(i) resample_holdout.data.frame(data, test = i)
  df <- as_tibble(transpose(map(split_kfold(seq_len(nrow(data)), k), f)))
  df[[id]] <- id(k)
  df
}

#' @export
crossv_kfold.grouped_df <- function(data, k = 1, ...) {
  f <- function(i) resample_holdout.grouped_df(data, test = i)
  df <- as_tibble(transpose(map(split_kfold(group_ids(data), k), f)))
  df[[id]] <- id(k)
  df
}