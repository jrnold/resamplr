#' Generate cross-validation test-training sets
#'
#' Generate cross-validation test/training pairs by specifying the indices
#' in the elements in the test and training sets.
#' This function provides a manual method for generating data frames with
#' with cross-validation \code{resample} objects, while other functions,
#' such as \code{\link{crossv_kfold}}, algorithmically handle generate the
#' indexes in the test- and training sets.
#'
#' @param expr A quosure
#' @param train A list of integer or character vectors
#' @param test A list of integer or character vector
#' @param extractor See \code{\link{lazy_sample}}.
#' @family cross-validation functions
#' @inherit crossv_kfold references
#' @export
crossv_idx <- function(expr, train, test, extractor = NULL) {
  tibble(train = lazy_sample_lst(expr, train, extractor = extractor),
         test = lazy_sample_lst(expr, test, extractor = extractor))
}
