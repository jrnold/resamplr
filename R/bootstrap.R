#' Generate bootstrap replicates
#'
#' Generate bootstrap replicates. In addition to the ordinary bootstrap,
#' it supports weighted bootstrap (\code{weights}),
#' Bayesian bootstrap (\code{bayes}), cluster bootstrap (if \code{data} is a
#' grouped data frame and \code{groups = TRUE}), and stratified bootstrap
#' (if \code{data} is grouped data frame and \code{stratify = TRUE}).
#'
#' @param data A data frame
#' @param R Number of replicates to sample
#' @param m Number of observations in the replicates.
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
#'
#' @seealso The \pkg{boot} function \code{\link[boot]{boot}} which is the
#'   canonical R bootstrap implementation. This function produces slightly different
#'   output and has more options than the \pkg{modelr} function
#'   \code{\link[modelr]{bootstrap}}.
#' @param bayes If \code{TRUE}, a Bayesian bootstrap is used.
#' @templateVar numrows \code{R} rows and
#' @template return_resample_df
#' @references
#' \itemize{
#' \item{Angelo Canty and Brian Ripley (2016). boot: Bootstrap R (S-Plus) Functions. R package version 1.3-18.}
#' \item{Davison, A. C. & Hinkley, D. V. (1997) Bootstrap Methods and Their Applications. Cambridge University Press, Cambridge. ISBN 0-521-57391-2}
#' }
#' @export
#' @example inst/examples/ex-bootstrap.R
bootstrap <- function(data, ...) {
  UseMethod("bootstrap")
}

#' @describeIn bootstrap Bootstrap rows in a data frame.
#' @export
bootstrap.data.frame <- function(data,
                                 R = 1L,
                                 weights = NULL,
                                 bayes = FALSE,
                                 m = nrow(data),
                                 ...) {
  if (!is.null(weights)) {
    assert_that(weights %in% names(data))
    weights <- data[[weights]]
  }
  to_resample_df(bootstrap_(nrow(data), R = R, weights = weights,
                            bayes = bayes, m = m), data)
}


#' @describeIn bootstrap Bootstraps a grouped data data frame. Allows for
#'   bootstrapping groups (clustered bootstrap) if \code{groups = TRUE}, and
#'   bootstrapping within groups (stratified bootstrap) if
#'   \code{stratify = TRUE}.
#' @importFrom assertthat is.string
#' @importFrom purrr map2_df rerun
#' @importFrom dplyr n_groups
#' @export
bootstrap.grouped_df <- function(data,
                                 R = 1L,
                                 stratify = FALSE,
                                 groups = TRUE,
                                 weights = NULL,
                                 bayes = FALSE,
                                 weight_groups = TRUE,
                                 weight_within = TRUE,
                                 ...) {
  assert_that(is.flag(stratify))
  assert_that(is.flag(groups))
  # One of these needs to be specified
  assert_that(stratify || groups)
  if (!is.null(weights)) {
    # fill in weights if none exist
    assert_that(weights %in% names(data))
    weights <- data[[weights]]
  }
  assert_that(is.flag(weight_groups))
  assert_that(is.flag(weight_within))
  idx <- group_indices_lst(data)
  # split weights by group
  if (!is.null(weights)) {
    weights <- map(idx, function(.x) weights[.x])
  } else {
    # ensure each group's weights sum to 1
    weights <- map(idx, function(i) {
      rep(1 / length(i), length(i))
    })
  }
  # sample by groups
  if (groups) {
    # calculate group level weights for easier lookup
    group_weights <- if (weight_groups) {
      purrr::map_dbl(weights, sum)
    } else {
      NULL
    }
    grps <- bootstrap_(n_groups(data), R = R, weights = group_weights,
                       bayes = bayes)
  } else {
    grps <- tibble(sample = rerun(R, seq_len(n_groups(data))),
                   .id = seq_len(R))
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
      bs <- bootstrap_(length(grp_idx), R = 1, weights = w, bayes = bayes)
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

bootstrap.default <- function(data,
                              R = 1L,
                              weights = NULL,
                              bayes = FALSE,
                              m = length(data),
                              ...) {
  assert_that(length(weights) == length(data))
  n <- length(data)
  out <- bootstrap_n(n, R = R, weights = weights, bayes = bayes, m = m)
  out[["sample"]] <- map(out[["sample"]], function(i) data[i])
  out
}

#' @export
#' @rdname
bootstrap_n <- function(n, R = 1L, weights = NULL, bayes = FALSE, m = n) {
  assert_that(is.number(n) && n >= 1)
  assert_that(is.number(R) && R >= 1)
  assert_that(is.numeric(weights))
  assert_that(is.flag(bayes))
  assert_that(is.number(m) && m >= 1)
  # resample weights
  if (bayes) {
    weights <- stats::rgamma(n, shape = weights %||% 1)
  }
  tibble(sample = rerun(R, sample.int(n, size = m, replace = TRUE, prob = weights)),
         .id = seq_len(R))
}
