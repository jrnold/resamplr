#' Generate permutation subsets
#'
#' Generate \code{R} replicates with permutations of the rows or groups of a data frame.
#'
#' @template param-expr
#' @template param-extractor
#' @template param-n
#' @param R The number of permutations to generate.
#' @param ... Arguments passed to methods
#' @templateVar numrows \code{R}
#' @templateVar f \code{permute}
#' @templateVar fn \code{permute_n}
#' @template return_lazy_sample_df
#' @export
permute <- function(expr, R = 1L, n = NULL, extractor = NULL) {
  expr <- enquo(expr)
  n <- n %||% idx_len(expr)
  out <- permute_n(n, R = R)
  out[["sample"]] <- lazy_sample_lst(UQ(expr), out[["sample"]], extractor = extractor)
  out
}

#' @rdname permute
#' @export
permute_n <- function(n, R = 1L) {
  n <- as.integer(n)
  assert_that(is_pos_scalar_integer(n))
  R <- as.integer(R)
  assert_that(is_pos_scalar_integer(n))
  tibble(sample = rerun(R, sample.int(n, replace = TRUE)))
}
