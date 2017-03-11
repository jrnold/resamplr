#' Generate permutations samples
#'
#' @param x A data frame
#' @param k The number of permutations to generate.
#' @param stratify If \code{TRUE}, the permute observations within each group.
#'   Otherwise permute the groups themselves.
#' @param ... Passed to methods
#' @return A data frame with \code{k} rows and the following columns:
#' \describe{
#' \item{sample}{A list of \code{\link{resample}} objects.}
#' \item{.id}{Character vector. ID of the sample.}
#' }
#' @export
permute <- function(x, ...) {
  UseMethod("permute")
}

# Note: this code is largely redundant and the same as crossv_loo
# It could be replaced by calling crossv_lpo, and then manipulating the returned
# data frame.

#' @rdname permute
#' @export
permute.data.frame <- function(x, k = 1L, ...) {
  to_resample_df(permute_(nrow(x), k = k), x)
}

#' @rdname permute
#' @export
permute.grouped_df <- function(x, k = 1L, stratify = TRUE, ...) {
  idx <- group_indices_lst(x)
  if (stratify) {
    f <- function(g) {
      mutate_(permute_(length(g), k),
              sample = ~ map(sample, function(i) g[i]))
    }
    res <- summarise_(group_by_(map_df(idx, f), ".id"),
                      sample = ~ list(flatten_int(sample)))
  } else {
    res <- map(permute_(length(idx), k = k),
               function(i) flatten_int(idx[i]))
    res[[".id"]] <- seq_len(k)
  }
  to_resample_df(res, x)[, c("sample", ".id")]
}

permute_ <- function(n, k = 1L, ...) {
  tibble(sample = purrr::rerun(k, sample.int(n, n, replace = FALSE)),
         .id = seq_len(k))
}
