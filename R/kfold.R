#' Generate cross-validated k-fold test-training pairs
#'
#' @inheritParams modelr::crossv_kfold
#' @param ... Passed to methods
#' @export
crossv_kfold <- function(data, k, ...) {
  UseMethod("crossv_kfold")
}

#'@export
crossv_kfold.data.frame <- function(data, k = 5, shuffle = shuffle, ....) {
  df <- as_tibble(transpose(split_kfold(seq_len(nrow(data)), shuffle = shuffle)))
  df[[".id"]] <- id(k)
  df
}

#' @export
crossv_kfold.grouped_df <- function(data, k = 5, shuffle = shuffle,
                                    stratify = FALSE, ...) {
  idx <- group_indices_lst(data)
  if (stratify) {
    g <- transpose(map(idx, split_kfold, k = k, shuffle = shuffle))
    test_train(data, test = flatten_int(map(g, "test")),
               train = flatten_int(map(g, "train")))
  } else {
    g <- split_kfold(seq_along(idx), test, train, shuffle = shuffle)
    test_train(data,
               test = flatten_int(idx[g$test]),
               train = flatten_int(idx[g$train]))
  }
}

split_n <- function(idx, k, shuffle = TRUE) {
  if (shuffle) {
    n <- length(idx)
    folds <- sample(rep(seq_len(k), length.out = n), n, replace = FALSE)
  } else {
    folds <- cut(idx, k, include.lowest = TRUE, labels = FALSE)
  }
  split(idx, folds)
}

split_kfold <- function(idx, k, shuffle = shuffle) {
  f <- function(i) list(train = setdiff(i, idx), test = i)
  map(split_n(idx, k, shuffle = shuffle), f)
}
