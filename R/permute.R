#' Generate permutation subsets
#'
#' Generate \code{R} replicates with permutations of the rows or groups of a data frame.
#'
#' @details If \code{R} is greater than the total number of permutations,
#'   only the total number of permutations is returned. Since the total number
#'   of permutations is \code{factorial(n)}, this will only happen if \code{n}
#'   is small.
#'
#' @param data A data frame
#' @param R The number of permutations to generate.
#' @param stratify If \code{TRUE}, the permute observations within each group.
#'   Otherwise permute the groups themselves.
#' @param ... Arguments passed to methods
#' @templateVar numrows \code{R} rows and
#' @template return_resample_df
#' @export
permute <- function(data, ...) {
  UseMethod("permute")
}

#' @describeIn permute Permute rows of a data frame.
#' @export
permute.data.frame <- function(data, R = 1L, ...) {
  assert_that(is.number(R) && R >= 1)
  to_resample_df(permute_(nrow(data), R = R), data)
}

#' @describeIn permute Permutes rows within each group in a grouped data frame
#'   if \code{stratify = TRUE}. Otherwise, permute the groups.
#' @export
permute.grouped_df <- function(data, R = 1L, stratify = FALSE, ...) {
  assert_that(is.number(R) && R >= 1 && R < Inf)
  # cannot do full permuatations
  assert_that(is.flag(stratify))
  idx <- group_indices_lst(data)
  if (stratify) {
    f <- function(g) {
      mutate_(permute_(length(g), R),
              sample = ~ map(sample, function(i) g[i]))
    }
    res <- summarise_(group_by_(map_df(idx, f), ".id"),
                      sample = ~ list(flatten_int(sample)))
  } else {
    res <- mutate_(permute_(length(idx), R = R),
                   sample = ~ map(sample, function(i) flatten_int(idx[i])))
  }
  to_resample_df(res, data)[, c("sample", ".id")]
}

permute_ <- function(n, R = 1L, force_random = FALSE) {
  # allow for full set of permutations if someone really wants it
  # return full set of permutations if R >= n!
  if (is.infinite(R) || (R >= factorial(n) && !force_random)) {
    tibble(sample = combinat::permn(n), .id = seq_along(sample))
  } else {
    tibble(sample = rerun(R, sample.int(n, n, replace = FALSE)),
           .id = seq_len(R))
  }
}
