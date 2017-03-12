#' Generate permutation samples
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

#' @rdname permute
#' @export
permute.data.frame <- function(x, k = 1L, ...) {
  assert_that(is.number(k) && k >= 1)
  to_resample_df(permute_(nrow(x), k = k), x)
}

#' @rdname permute
#' @export
permute.grouped_df <- function(x, k = 1L, stratify = FALSE, ...) {
  assert_that(is.number(k) && k >= 1 && k < Inf)
  # cannot do full permuatations
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(x)
  if (stratify) {
    f <- function(g) {
      mutate_(permute_(length(g), k),
              sample = ~ map(sample, function(i) g[i]))
    }
    res <- summarise_(group_by_(map_df(idx, f), ".id"),
                      sample = ~ list(flatten_int(sample)))
  } else {
    res <- mutate_(permute_(length(idx), k = k),
                   sample = ~ map(sample, function(i) flatten_int(idx[i])))
  }
  to_resample_df(res, x)[, c("sample", ".id")]
}

permute_ <- function(n, k = 1L, force_random = FALSE) {
  # allow for full set of permutations if someone really wants it
  # return full set of permutations if k >= n!
  if (is.infinite(k) || (k >= factorial(n) && !force_random)) {
    tibble(sample = combinat::permn(n), .id = seq_along(sample))
  } else {
    tibble(sample = purrr::rerun(k, sample.int(n, n, replace = FALSE)),
           .id = seq_len(k))
  }
}
