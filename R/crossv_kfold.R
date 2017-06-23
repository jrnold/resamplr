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

#' @export
crossv_kfold.default <- function(data, K = 5L, shuffle = TRUE, ...) {
  out <- crossv_kfold(resample_idx_len(data), K = K, shuffle = shuffle)
  for (i in c("test", "train")) {
    out[[i]] <- resample_lst(data, out[[i]])
  }
  out
}

#' @export
crossv_kfold.grouped_df <- function(data, K = 5L, shuffle = TRUE, ...) {
  out <- crossv_kfold(resample_idx_len(data, groups = TRUE),
                      K = K, shuffle = shuffle)
  for (i in c("test", "train")) {
    out[[i]] <- resample_lst(data, out[[i]], groups = TRUE)
  }
  out
}

crossv_kfold_1fold <- function(idx, test) {
  tibble(train = list(setdiff(idx, test)), test = list(test))
}

#' @importFrom purrr map_df
crossv_kfold_ <- function(n, K = 5L, shuffle = TRUE) {
  folds <- partition(seq_len(n), as.integer(K), shuffle = shuffle)
  map_df(folds, crossv_kfold_1fold)
}

#' Partition a vector
#'
#' Partition a vector \code{x} into \code{K} sets, optionally shuffling the
#' values of \code{x} prior to partitioning.
#'
#' @param x A vector
#' @param K int The number of partitions
#' @param shuffle logical.
#' @return A length \code{K} list of vectors of the same type as \code{x}
#' @noRd
partition <- function(x, K, shuffle = TRUE) {
  if (shuffle) {
    n <- length(x)
    folds <- sample(rep(seq_len(K), length.out = n), size = n, replace = FALSE)
  } else {
    folds <- cut(x, K, include.lowest = TRUE, labels = FALSE)
  }
  split(x, folds)
}
