#' Leave-one-out cross-validation
#'
#' Leave one out cross-validation. This is a convenience function
#' for \code{\link[modelr]{crossv_kfold}} when \code{k = nrow(data)}.
#'
#' @param data A data frame
#' @param id Name of variable that gives each model a unique integer id.
#' @param p Number of objects to leave out
#' @param ... Arguments passed to methods
#' @return A data frame with n rows and columns \code{test} and \code{train}.
#'   \code{test} and \code{train} are list-columns containing
#'   \code{\link{resample}} objects.
#' @importFrom modelr crossv_kfold
#' @export
#' @examples
#' cv <- crossv_loo(mtcars)
#' cv
#' require("purrr")
#' require("modelr")
#' models <- map(cv$train, ~ lm(mpg ~ wt, data = .))
#' errs <- map2_dbl(models, cv$test, rmse)
#' hist(errs)
crossv_loo <- function(data, id = ".id", ...) {
  UseMethod("crossv_loo")
}

#' @describeIn crossv_loo Leave-one-out cross validation
#' @export
crossv_loo.data.frame <- function(data, id = ".id", ...) {
  df <- transpose(map(seq_len(nrow(data)), function(i) {
    resample_holdout(data, test = i)
  }))
  df <- tibble::as_tibble(df)
  df[[id]] <- id(nrow(df))
  df
}

#' @describeIn crossv_loo Leave-one-group out cross validation
#' @export
crossv_loo.grouped_df <- function(data, id = ".id", ...) {
  df <- transpose(map(group_ids(data), function(i) {
    resample_holdout.grouped_df(data, test = i)
  }))
  df <- tibble::as_tibble(df)
  df[[id]] <- id(nrow(df))
  df
}

#' @rdname crossv_loo
#' @export
crossv_lpo <- function(data, ...) {
  UseMethod("crossv_lpo")
}

#' @describeIn crossv_loo Leave-p-out out cross validation
#' @export
crossv_lpo.data.frame <- function(data, p = 1, id = ".id", ...) {
  if (p == 1) {
    crossv_loo.data.frame(data, id)
  } else {
    idx <- seq_len(nrow(data))
    df <- transpose(map(utils::combn(idx, p, simplify = FALSE),
                        function(i) resample_holdout(data, i)))
    df <- tibble::as_tibble(df)
    df[[id]] <- id(nrow(df))
    df
  }
}

#' @describeIn crossv_loo Leave-p-groups out cross validation
#' @export
crossv_lpo.grouped_df <- function(data, p = 1, id = ".id", ...) {
  if (p == 1) {
    crossv_loo.grouped_df(data, id)
  } else {
    df <- transpose(map(utils::combn(group_ids(data), p, simplify = FALSE),
                        function(i) resample_holdout.grouped_df(data, i)))
    df <- tibble::as_tibble(df)
    df[[id]] <- id(nrow(df))
    df
  }
}

