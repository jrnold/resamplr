#' Generate a permutation replicate
#'
#' Generate a permutation replicate, which randomly shuffles the rows of
#' the data frame.
#'
#' @param data A data frame
#' @return A \code{\link[modelr]{resample}} object.
#' @export
resample_permutation <- function(data) {
  modelr::resample(data = data, idx = sample.int(nrow(data), replace = FALSE))
}

#' Generate permutation replicates
#'
#' Generate permutation replicates, which randomly shuffles the row of
#' the data frame.
#'
#' @param n Number of replicates to draw
#' @param data A data frame
#' @param id Name of variable that gives each model a unique integer id.
#' @return A data frame with a single column named \code{perm}.
#'   This column is a list column of \code{\link[modelr]{resample}} objects.
#' @export
permute <- function(data, n, id = ".id") {
  perm <- purrr::rerun(n, resample_permutation(data))
  df <- tibble::tibble(perm = perm)
  df[[id]] <- id(nrow(df))
  df
}