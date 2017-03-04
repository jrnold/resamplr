#' Generate a permutation replicate
#'
#' Generate a permutation replicate, which randomly shuffles the rows of
#' the data frame.
#'
#' @param data A data frame
#' @param stratify If \code{TRUE}, permute rows within groups
#' @param groups If \code{TRUE}, permute groups
#' @return A \code{\link[modelr]{resample}} object.
#' @export
resample_permutation <- function(data, ...) {
  UseMethod("resample_permutation")

}

#' @describeIn resample_permutation Permute observations
#' @export
resample_permutation.data.frame <- function(data, ...) {
  idx <- sample.int(nrow(data), replace = FALSE)
  resample(data = data, idx = idx)
}

#' @describeIn resample_permutation Permute groups or observations within
#'    groups.
#' @export
#' @importFrom dplyr n_groups
resample_permutation.grouped_df <- function(data,
                                            stratify = TRUE,
                                            groups = FALSE, ...) {
  g <- if (groups) {
    sample.int(n_groups(data), replace = FALSE)
  } else {
    NULL
  }
  idx <- get_group_indexes_int(data, g)
  resample(data = data, idx = idx)
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