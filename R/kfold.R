#' Generate cross-validated K-fold test-training pairs
#'
#' Generate cross-validated K-fold test-training pairs. In addition to ordinary
#' K-fold cross-validation, this supports stratified K-fold cross validation
#' if \code{data} is a grouped data frame and \code{stratify = TRUE}, and
#' Group K-fold if \code{data} is a grouped data frame and \code{stratify = FALSE}.
#'
#' @param data A data frame
#' @param K The number of folds
#' @param shuffle If \code{TRUE}, randomly assign observations to folds.
#'   Otherwise, observations are sequentially assigned to folds.
#' @param stratify If \code{TRUE}, within each group observations are split
#'   into folds, and those folds combined. If \code{FALSE}, groups are assigned
#'   into folds.
#' @param ... Arguments passed to methods
#' @templateVar numrows \code{K} rows and
#' @template return_crossv_df
#' @seealso This function has more features than the \pkg{modelr} function
#'   \code{\link[modelr]{crossv_kfold}}.
#' @references
#' \itemize{
#' \item{Breiman, L., Friedman, J.H., Olshen, R.A. and Stone, C.J. (1984) Classification and Regression Trees. Wadsworth.}
#' \item{Burman, P. (1989) A comparative study of ordinary cross-validation, v-fold cross-validation and repeated learning-testing methods. Biometrika, 76, 503–514}
#' \item{Davison, A.C. and Hinkley, D.V. (1997) Bootstrap Methods and Their Application. Cambridge University Press.}
#' \item{Efron, B. (1986) How biased is the apparent error rate of a prediction rule? Journal of the American Statistical Association, 81, 461–470.}
#' \item{Stone, M. (1974) Cross-validation choice and assessment of statistical predictions (with Discussion). Journal of the Royal Statistical Society, B, 36, 111–147.}
#' }
#' @export
#' @example inst/examples/ex-crossv_kfold.R
crossv_kfold <- function(data, K, ...) {
  UseMethod("crossv_kfold")
}

#' @describeIn crossv_kfold Splits rows in a data frame into folds.
#' @export
crossv_kfold.data.frame <- function(data, K = 5L, shuffle = TRUE, ...) {
  to_crossv_df(crossv_kfold_(nrow(data), K = K, shuffle = shuffle), data)
}

#' @describeIn crossv_kfold Partitions rows within each group of a grouped data frame
#'   into folds if \code{stratify = FALSE}. This ensures that the test and training
#'   sets will have approximately equal proportions of each group. If \code{stratify = TRUE},
#'   then the groups are partitioned into folds.
#' @importFrom dplyr summarise_ group_by_ mutate_
#' @importFrom purrr map_df
#' @export
crossv_kfold.grouped_df <- function(data, K = 5L, shuffle = TRUE,
                                    stratify = FALSE, ...) {
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(data)
  if (stratify) {
    f <- function(g) {
      mutate_(crossv_kfold_(length(g), K = K, shuffle = shuffle),
              train = ~ map(train, function(i) g[i]),
              test = ~ map(test, function(i) g[i]))
    }
    res <- summarise_(group_by_(map_df(idx, f), ".id"),
                      train = ~ list(flatten_int(train)),
                      test = ~ list(flatten_int(test)))
  } else {
    res <- mutate_(crossv_kfold_(length(idx), K, shuffle = shuffle),
                   train = ~ map(train, function(i) flatten_int(idx[i])),
                   test = ~ map(test, function(i) flatten_int(idx[i])))
    res
  }
  to_crossv_df(res, data)[, c("train", "test", ".id")]
}

crossv_kfold_ <- function(n, K = 5L, shuffle = TRUE, ...) {
  x <- seq_len(n)
  f <- function(i, .id) {
    tibble(train = list(setdiff(x, i)), test = list(i), .id = .id)
  }
  folds <- partition(x, as.integer(K), shuffle = shuffle)
  map2_df(folds, seq_along(folds), f)
}

partition <- function(x, K, shuffle = TRUE) {
  if (shuffle) {
    n <- length(x)
    folds <- sample(rep(seq_len(K), length.out = n), size = n, replace = FALSE)
  } else {
    folds <- cut(x, K, include.lowest = TRUE, labels = FALSE)
  }
  split(x, folds)
}
