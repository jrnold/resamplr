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
#' @template param-extractor
#' @family cross-validation functions
#' @inherit crossv_kfold references
#' @return A data frame with <%= numrows %> rows and the following columns:
#' \describe{
#' \item{train}{A list column with objects representing the training sets. The list elements are \code{\link{lazy_sample}} objects.}
#' \item{test}{A list column with objects representing the training sets. The list elements are \code{\link{lazy_sample}} objects.}
#' }
#' @example inst/examples/ex-crossv_idx.R
#' @export
crossv_idx <- function(expr, train, test, extractor = NULL) {
  expr <- enquo(expr)
  tibble(train = lazy_sample_lst(UQ(expr), train, extractor = extractor),
         test = lazy_sample_lst(UQ(expr), test, extractor = extractor))
}
