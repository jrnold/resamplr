#' Generate cross-validated time-series k-fold test-training pairs
#'
#' @inheritParams modelr::crossv_kfold
#' @param ... Passed to methods
#' @export
crossv_tskfold <- function(data, k, ...) {
  if (!is.numeric(k) || length(k) != 1) {
    stop("`k` must be a single integer.", call. = FALSE)
  }
  UseMethod("crossv_tskfold")
}

#' @export
crossv_tskfold.data.frame <- function(data, k = 1, cumtrain = TRUE,
                                      cumtest = FALSE) {
  f <- function(x) {
     resample_holdout.data.frame(data, test = x[["test"]], train = x[["train"]])
  }
  folds <- split_tskfold(seq_len(nrow(data)), k = k, cumtrain = cumtrain,
                         cumtest = cumtest)
  df <- as_tibble(transpose(map(folds, f)))
  df[[id]] <- id(k)
  df
}

#' @export
crossv_tskfold.grouped_df <- function(data, k = 1, cumtrain = TRUE,
                                      cumtest = FALSE) {
  f <- function(x) {
    resample_holdout.grouped_df(data, test = x[["test"]], train = x[["train"]])
  }
  folds <- split_tskfold(group_ids(data), k = k, cumtrain = cumtrain,
                         cumtest = cumtest)
  df <- as_tibble(transpose(map(folds, f)))
  df[[id]] <- id(k)
  df
}


split_tskfold <- function(x, k, cumtrain = TRUE, cumtest = TRUE) {
  UseMethod("split_tskfold")
}

split_tskfold.default <- function(x, k, cumtrain = TRUE, cumtest = TRUE) {
  g <- cut(idx, k, include.lowest = TRUE, labels = FALSE)
  idx_list <- split(idx, g)
  map(seq_len(k), function(i) {
    test <- if (cumtest) idx_list[(i + 1):k] else idx_list[i + 1]
    train <- if (cumtest) idx_list[1:i] else idx_list[i]
    list(train = train, test = test)
  })
}

split_tskfold.data.frame <- function(x, k, cumtrain = TRUE, cumtest = FALSE) {
  split_tskfold(seq_len(nrow(x)), k = k, cumtrain = cumtrain,
                cumtest = cumtest)
}

split_tskfold.data.frame <- function(x, k, cumtrain = TRUE, cumtest = FALSE) {
  folds <- split_tskfold(seq_len(n_groups(x)), k = k, cumtrain = cumtrain,
                         cumtest = cumtest)
  f <- function(g) get_group_indexes_int(x, g)
  map(folds, f)
}
