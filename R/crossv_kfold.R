#' Generate cross-validated k-fold test-training pairs
#'
#' @inheritParams modelr::crossv_kfold
#' @param ... Passed to methods
#' @export
crossv_kfold <- function(data, ...) {
  UseMethod("crossv_kfold")
}

#'@export
crossv_kfold.data.frame <- function(data, k = 5, id = ".id", ...) {
  modelr::crossv_kfold(data, k = k, id = id)
}

#' @export
crossv_kfold.grouped_df <- function(data, k = 1, id = ".id", ...) {
  if (!is.numeric(k) || length(k) != 1) {
    stop("`k` must be a single integer.", call. = FALSE)
  }
  df <- as_tibble(transpose(map(rpartition(group_ids(data), k), function(i) {
    resample_holdout.grouped_df(data, test = i)
  })))
  df[[id]] <- id(k)
  df
}