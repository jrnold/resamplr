#' Generate cross-validated K-fold test-training pairs
#'
#' Generate cross-validated K-fold test-training pairs. In addition to ordinary
#' K-fold cross-validation, this supports stratified K-fold cross validation
#' if \code{data} is a grouped data frame and \code{stratify = TRUE}, and
#' Group K-fold if \code{data} is a grouped data frame and \code{stratify = FALSE}.
#'
#' @template param-expr
#' @param K An integer scalar. The number of cross-validation folds.
#' @param shuffle A logical scalar. If \code{TRUE}, then the elements are randomly shuffled prior to partitioning into folds.
#' @template param-n
#' @param ... Arguments passed to methods
#' @seealso The \pkg{boot} function \code{\link[boot]{cv.glm}} and
#' \pkg{modelr} function \code{\link[modelr]{crossv_kfold}} for
#' other implementations of k-fold cross-validation.
#'
#' @templateVar numrows \code{K}
#' @templateVar f \code{crossv_kfold}
#' @templateVar fn \code{crossv_kfold_n}
#' @template return_crossv_df
#'
#' @references
#' \itemize{
#' \item{Breiman, L., Friedman, J.H., Olshen, R.A. and Stone, C.J. (1984) Classification and Regression Trees. Wadsworth.}
#' \item{Burman, P. (1989) A comparative study of ordinary cross-validation, v-fold cross-validation and repeated learning-testing methods. Biometrika, 76, 503–514}
#' \item{Davison, A.C. and Hinkley, D.V. (1997) Bootstrap Methods and Their Application. Cambridge University Press.}
#' \item{Efron, B. (1986) How biased is the apparent error rate of a prediction rule? Journal of the American Statistical Association, 81, 461–470.}
#' \item{Stone, M. (1974) Cross-validation choice and assessment of statistical predictions (with Discussion). Journal of the Royal Statistical Society, B, 36, 111–147.}
#' }
#' @export
crossv_kfold <- function(expr, K, ...) {
  expr <- enquo(expr)
  out <- crossv_kfold_n(idx_len(expr), K = K, ...)
  for (i in c("test", "train")) {
    out[[i]] <- lazy_sample_lst(!!expr, out[[i]])
  }
  out
}

#' @rdname crossv_kfold
#' @importFrom purrr map_df
crossv_kfold_n <- function(n, K = 5L, shuffle = TRUE) {
  idx <- seq_len(n)
  test <- partition(idx, as.integer(K), shuffle = shuffle)
  train <- map(test, setdiff, x = idx)
  tibble(train = train, test = test)
}

#' Partition a vector
#'
#' Partition a vector \code{x} into \code{K} sets, optionally shuffling the
#' values of \code{x} prior to partitioning.
#'
#' @param x A vector
#' @param K An integer scalar. The number of partitions.
#' @template param-n
#' @param shuffle A logical scalar. If \code{TRUE}, then randomly shuffle the elements prior to partitioning.
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
