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
#' @export
permute <- function(data, ...) {
  UseMethod("permute")
}

#' @describeIn permute Permute rows of a data frame.
#' @export
permute.data.frame <- function(data, R = 1L, ...) {
  assert_that(is.number(R) && R >= 1)
  out <- permute_(nrow(data), R = R)
  out[["sample"]] <- resample_lst(data, out[["sample"]])
  out
}

#' @describeIn permute Permutes rows within each group in a grouped data frame
#'   if \code{stratify = TRUE}. Otherwise, permute the groups.
#'
#' @importFrom purrr flatten
#' @export
permute.grouped_df <- function(data, R = 1L, groups = TRUE, within = 0,
                               ...) {
  assert_that(is.flag(groups))
  assert_that(is.flag(within))
  indices <- attr(data, "indices")
  G <- length(indices)
  # list of lists of integer vectors
  if (groups) {
    grps_permn <- map(permute_(G, R, ...)[["sample"]], function(i) indices[i])
  } else {
    grps_permn <- list(indices)
  }
  # permute within a group
  permute_within_group <- function(.idx, R = 1L) {
    map(permute_(length(.idx), R = R)[["sample"]], function(i) .idx[i])
  }
  permute_within_all_groups <- function(.gidx, R = 1L) {
    map(transpose(map(.gidx, permute_within_group, R = R)),
        function(.x) tibble(sample = flatten_int(.x)))
  }
  todf <- function(.x) tibble(sample = flatten_int(.x))
  if (within > 0) {
    flatten(map(grp_permn, permute_within_all_groups, R = within))
  } else {
    tibble(sample = map(grps_permn, todf))
  }
}

permute_ <- function(n, R = 1L, force_random = FALSE) {
  # allow for full set of permutations if someone really wants it
  # return full set of permutations if R >= n!
  logn_permn <- lfactorial(n)
  if (is.infinite(R) || (log(R) >= logn_permn && !force_random)) {
    if (logn_permn >= log(8)) {
      message("The total number of permutations is very large: ",
              exp(logn_permn))
    }
    tibble(sample = combinat::permn(n), seq_along(sample))
  } else {
    tibble(sample = rerun(R, sample.int(n, n, replace = FALSE)))
  }
}
