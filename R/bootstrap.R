#' Generate a bootstrap
#'
#' @param data A data frame
#' @param ... Passed to methods
#' @param within Resample within groups (stratified bootstrap)
#' @param groups Resample groups (clustered bootstrap)
#' @return A \code{\link[modelr]{resample}} object.
#' @export
resample_bootstrap <- function(data, ...) {
  UseMethod("resample_bootstrap")
}

#' @export
resample_bootstrap.data_frame <- function(data, ...) {
  modelr::resample(data, sample.int(nrow(data), replace = TRUE))
}

#' @export
resample_bootstrap.grouped_df <- function(data, within = TRUE, groups = FALSE,
                                          ...) {
  idx <- attr(data, "indices")
  # resample the groups
  if (groups) {
    idx <- idx[sample.int(length(idx), size = length(idx), replace = TRUE)]
  }
  # resample within groups
  if (within) {
    idx <- purrr::map(indices, function(x) {
      sample(x, size = length(x), replace = TRUE)
    })
  }
  idx <- purrr::flatten_int(idx)
  modelr::resample(data, idx)
}

#' Generate n bootstrap replicates
#'
#' Generate n bootstrap replicates
#'
#' @param data A data frame
#' @param n Number of bootstrap replicates to generate
#' @param ... Passed to \code{\link{resample_bootstrap}}
#' @param id Name of variable that gives each model a unique integer id
#' @return A data frame with two columns and \code{n} rows
#' \describe{
#' \item{strap}{A list column of \code{\link[modelr]{objects}}}
#' \item{id}{The replicate identifier}
#' }
#' @export
bootstrap <- function(data, n, ..., id = ".id") {
  bootstrap <- purrr::rerun(n, resample_bootstrap(data, ...))
  df <- tibble::data_frame(strap = bootstrap)
  df[[id]] <- id(n)
  df
}
