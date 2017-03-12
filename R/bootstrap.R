#' Generate bootstrap replicates
#'
#' Generate bootstrap replicates. In addition to the ordinary bootstrap,
#' it supports weighted bootstrap (\code{weights}),
#' Bayesian bootstrap (\code{bayes}), cluster bootstrap (if \code{data} is a
#' grouped data frame and \code{groups = TRUE}), and stratified bootstrap
#' (if \code{data} is grouped data frame and \code{stratify = TRUE}).
#'
#' @param data A data frame
#' @param k Number of replicates to sample
#' @param ... Passed to methods
#' @param stratify Resample within groups (stratified bootstrap)
#' @param groups Resample groups (clustered bootstrap)
#' @param weights Observation level weights. This must be the name of a column
#'    in \code{data}.
#' @param weight_groups If \code{TRUE} and \code{groups = TRUE},
#'    then sample groups using group-level weights.
#'    If \code{weights} is \code{NULL}, then weight groups by the number
#'    of observations in each. If \code{weights}, is not-\code{NULL}, then
#'    use the sum of the observation level weights in each group as the
#'    group-level weight.
#' @param weight_within If \code{TRUE} and \code{stratify = TRUE}, then
#'    use observation level weights to sample observations within each group.
#' @param bayes If \code{TRUE}, a Bayesian bootstrap is used.
#' @return A data frame with \code{k} rows and columns:
#' \describe{
#' \item{\code{sample}}{A list of \code{\link{resample}} objects with
#' with the bootstrap replicates.}
#' \item{\code{.id}}{An integer vector with replicate ids}
#' }
#' @export
bootstrap <- function(data, ...) {
  UseMethod("bootstrap")
}

#' @describeIn bootstrap Bootstrap rows in a data frame.
#' @export
bootstrap.data.frame <- function(data, k = 1L,
                                 weights = NULL,
                                 bayes = FALSE,
                                 ...) {
  assert_that(is.null(weights) || is.string(weights))
  weights <- if (is.null(weights)) NULL else data[[weights]]
  to_resample_df(bootstrap_(nrow(data), k = k, weights = weights,
                            bayes = bayes),
                 data)
}


#' @describeIn bootstrap Bootstraps a grouped data data frame. Allows for
#'   bootstrapping groups (clustered bootstrap) if \code{groups = TRUE}, and
#'   bootstrapping within groups (stratified bootstrap) if
#'   \code{stratify = TRUE}.
#' @importFrom assertthat is.string
#' @importFrom purrr map2_df
#' @importFrom dplyr n_groups
#' @export
bootstrap.grouped_df <- function(data, k = 1L,
                                  stratify = FALSE,
                                  groups = TRUE,
                                  weights = NULL,
                                  weight_groups = TRUE,
                                  weight_within = TRUE,
                                  bayes = FALSE, ...) {
  assert_that(is.flag(stratify))
  assert_that(is.flag(groups))
  assert_that(is.null(weights) || is.string(weights))
  assert_that(is.flag(weight_groups))
  assert_that(is.flag(weight_within))
  idx <- group_indices_lst(data)
  # fill in weights if none exist
  # split weights by groups for easier lookup
  if (is.null(weights)) {
    # ensure that by default groups are not weighted
    weights <- map(idx, function(i) {
      rep(1 / length(i), length(i))
    })
  } else {
    weights <- data[[weights]]
    weights <- map(idx, ~ weights[.x])
  }
  # calculate group level weights for easier lookup
  group_weights <- if (weight_groups) {
    purrr::map_dbl(weights, sum)
  } else {
    rep(1, dplyr::n_groups(data))
  }
  # sample by groups
  if (groups) {
    grps <- bootstrap_(n_groups(data), k = k, weights = group_weights,
                       bayes = bayes)
  } else {
    grps <- tibble(sample = purrr::rerun(k, seq_len(n_groups(data))),
                   .id = seq_len(k))
  }
  # resample within a group
  f1 <- function(g, .id) {
    grp_idx <- idx[[g]]
    # If stratified, then resample
    if (stratify) {
      if (weight_within) {
        w <- weights[[g]]
      } else {
        w <- rep(1, length(grp_idx))
      }
      bs <- bootstrap_(length(grp_idx), k = 1, weights = w, bayes = bayes)
      grp_idx <- grp_idx[bs[["sample"]][[1]]]
    }
    tibble(sample = grp_idx, .group = rep(.id, length(grp_idx)))
  }
  # resample within all groups in a replicate and combine
  f2 <- function(s, .id) {
    res <- map2_df(s, seq_along(s), f1)
    summarise_(res,
               sample = ~ list(sample),
               .group = ~ list(.group),
               .id = ~ .id)
  }
  to_resample_df(map2_df(grps[["sample"]], seq_along(grps[["sample"]]), f2),
                 data)
}


bootstrap_ <- function(n, k = 1L, weights = NULL, bayes = FALSE) {
  assert_that(is.number(k) && k >= 1)
  assert_that(is.null(weights) ||
                (is.numeric(weights) && length(weights) %in% c(1, n)))
  assert_that(is.flag(bayes))
  # resample weights
  if (bayes) {
    weights <- if (is.null(weights)) {
      1
    } else {
      (weights / sum(weights)) * n
    }
    weights <- stats::rgamma(n, shape = weights)
  }
  f <- function(i) {
    tibble(sample = list(sample.int(n, size = n, replace = TRUE,
                                    prob = weights)),
           .id = i)
  }
  map_df(seq_len(k), f)
}
