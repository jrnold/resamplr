#' Leave-on-out cross-validation
#'
#' Leave one out cross-validation. This is a convenience function
#' for \code{\link[modelr]{crossv_kfold}} when \code{k = nrow(data)}.
#'
#' @param data A data frame
#' @param id Name of variable that gives each model a unique integer id.
#' @return A data frame with n rows and columns \code{test} and \code{train}.
#'   \code{test} and \code{train} are list-columns containing
#'   \code{\link{resample}} objects.
#'
#' @importFrom modelr crossv_kfold
#' @export
#' @example
#'
#' cv <- crossv_loo(mtcars)
#' cv
#' library(purrr)
#' models <- map(cv$train, ~ lm(mpg ~ wt, data = .))
#' errs <- map2_dbl(models, cv$test, rmse)
#' hist(errs)
crossv_loo <- function(data, id = ".id") {
  UseMethod("crossv_loo")
}

resample_lo <- function(data, ...) {
  UseMethod("resample_lo")
}

resample_lo.data.frame <- function(data, idx) {
  idx <- nrow(idx)
  list(train = resample(data, setdiff(idx, i)), test = modelr::resample(data, idx))
}

resample_lo.grouped_df <- function(data, id) {
  grp_idxs <- group_indices(data)
  test_idx <- which[id %in% group_idx]
  train_idx <- setdiff(seq_len(nrow(data)), test_idx)
  list(train = resample(data, train_idx), test = resample(data, test_idx))
}


#' @describeIn Each observation is left out
#' @export
crossv_loo.data.frame <- function(data, id = ".id") {
  df <- map_df(seq_len(nrow), function(i) resample_lo.data.frame(data, i))
  df[[id]] <- id(nrow(df))
  df
}

#' @describeIn Each group is left out
#' @export
crossv_loo.grouped_df <- function(data, id = ".id") {
  df <- map_df(seq_len(nrow), function(i) resample_lo.grouped_df(data, i))
  df[[id]] <- id(nrow(df))
  df
}

#' @describeIn Each observation is left out
#' @export
crossv_lpo.data.frame <- function(data, p = 1, id = ".id") {
  if (p == 1) {
    crossv_loo.data.frame(data, id)
  } else {
    idx <- seq_len(nrow(data))
    df <- purrr::map_df(combn(idx, p, simplify = FALSE),
                        function(i) resample_lo.data.frame(data, i))
    df[[id]] <- id(nrow(df))
    df
  }
}

#' @describeIn Each group is left out
#' @export
crossv_lpo.grouped_df <- function(data, id = ".id") {
  grps <- seq_len(n_groups(data))
  if (p == 1) {
    crossv_loo.grouped_df(data, id)
  } else {
    df <- purrr::map_df(combn(grps, p, simplify = FALSE),
                  function(i) resample_lo.grouped_df(data, i))
    df[[id]] <- id(nrow(df))
    df
  }
}

crossv_kfold.grouped_df <- function(data, k) {
  if (!is.numeric(k) || length(k) != 1) {
    stop("`k` must be a single integer.", call. = FALSE)
  }
  n <- n_groups(data)
  folds <- sample(rep(1:k, length.out = n))
  idx <- seq_len(n)
  fold_grps <- split(idx, folds)
  fold <- function(test) {
    list(train = resample(data, setdiff(idx, test)),
         test = resample(data, test))
  }
  cols <- purrr::transpose(purrr::map(fold_idx, fold))
  cols[[id]] <- id(k)
  tibble::as_data_frame(cols)
}