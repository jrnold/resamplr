#' Generate cross-validated test-training pairs
#'
#' @param data A data frame
#' @param n Number of replicates
#' @param test Proportion of observation in the test sample
#' @param stratify If \code{TRUE}, then test-train splits are done
#'   within each group so that the overall test-train split has
#'   approximately equal proportions of each group. If \code{FALSE},
#'   then test-train splits by groups.
#' @param ... Arguments passed to methods
#' @return A data frame with \code{n} rows and the following columns:
#' \describe{
#' \item{train}{A list of \code{\link{resample}} objects. Training sets.}
#' \item{test}{A list of \code{\link{resample}} objects. Test sets.}
#' \item{.id}{Character vector. ID of the fold.}
#' }
#' @export
crossv_mc <- function(data, n, ...) {
  UseMethod("crossv_mc")
}

crossv_mc_ <- function(data, n, ...) {
  df <- as_tibble(transpose(purrr::rerun(n, resample_holdout(data, ...))))
  df[[".id"]] <- id(nrow(df))
  df
}

#' @rdname crossv_mc
#' @export
crossv_mc.data.frame <- function(data, n, test = 0.3, ...) {
  crossv_mc_(data, n, test = test)
}

#' @rdname crossv_mc
#' @export
crossv_mc.grouped_df <- function(data, n, test = 0.3, stratify = FALSE, ...) {
  crossv_mc_(data, n, test = test, stratify = stratify)
}
