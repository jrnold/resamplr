#' Repeated k-fold cross-validation
#'
#' Repeated k-fold cross-validation.
#'
#' @param data A data frame
#' @param id Name of variable that gives each model a unique integer id.
#' @return A data frame with n rows and columns \code{test} and \code{train}.
#'   \code{test} and \code{train} are list-columns containing
#'   \code{\link{resample}} objects.
#'
#' @importFrom modelr crossv_kfold
#' @importFrom dplyr bind_rows
#' @importFrom purrr rerun
#' @inheritParams modelr::crossv_kfold
#' @export
#' @example
#'
#' cv <- crossv_repeated(mtcars, 10, 5)
#' cv
#' library(purrr)
#' models <- map(cv$train, ~ lm(mpg ~ wt, data = .))
#' errs <- map2_dbl(models, cv$test, rmse)
#' hist(errs)
crossv_repeated <- function(data, k = 5, n = 1, id = ".id") {
  bind_rows(rerun(n, crossv_kfold(data, k = k, id = id)))
}
