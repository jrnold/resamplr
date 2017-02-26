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
  crossv_kfold(data, nrow(data), id = id)
}
