#' Generate a bootstrap replicate
#'
#' @param data A data frame
#' @param ... Passed to methods
#' @param stratify Resample within groups (stratified bootstrap)
#' @param groups Resample groups (clustered bootstrap)
#' @return A \code{\link[modelr]{resample}} object.
#' @export
resample_bootstrap <- function(data, ...) {
  UseMethod("resample_bootstrap")
}

#' @rdname resample_bootstrap
#' @export
resample_bootstrap.data.frame <- function(data, ...) {
  resample(data, sample.int(nrow(data), replace = TRUE))
}


#' @rdname resample_bootstrap
#' @export
resample_bootstrap.grouped_df <- function(data, stratify = FALSE, groups = TRUE,
                                          ...) {
  assert_that(is.flag(stratify))
  assert_that(is.flag(groups))
  idx <- group_indices_lst(data)
  # resample the groups
  if (groups) {
    idx <- idx[sample.int(length(idx), size = length(idx), replace = TRUE)]
  }
  # resample within groups
  # these are not mutually exclusive
  if (stratify) {
    idx <- map(idx, function(x) {
      sample(x, size = length(x), replace = TRUE)
    })
  }
  idx <- flatten_int(idx)
  resample(data, idx)
}

#' Generate n bootstrap replicates
#'
#' Generate n bootstrap replicates
#'
#' @param data A data frame
#' @param n Number of bootstrap replicates to generate
#' @param ... Passed to \code{\link{resample_bootstrap}}
#' @return A data frame with two columns and \code{n} rows
#' \describe{
#' \item{strap}{A list column of \code{\link[modelr]{resample}} objects.}
#' \item{id}{The replicate identifier}
#' }
#' @export
#' @example inst/examples/bootstrap.R
bootstrap <- function(data, n = 1, ...) {
  assert_that(is.number(n) && n > 0)
  n <- as.integer(n)
  tibble(
    strap = purrr::rerun(n, resample_bootstrap(data, ...)),
    .id = id(n)
  )
}
