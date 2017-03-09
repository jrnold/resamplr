#' Generate a bootstrap replicate
#'
#' @param data A data frame
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
#' @return A \code{\link[modelr]{resample}} object.
#' @export
resample_bootstrap <- function(data, ...) {
  UseMethod("resample_bootstrap")
}

#' @rdname resample_bootstrap
#' @export
resample_bootstrap.data.frame <- function(data,
                                          weights = NULL,
                                          bayes = FALSE,
                                          ...) {
  assert_that(is.data.frame(data))
  assert_that(is.null(weights) || is.string(weights))
  assert_that(is.flag(bayes))
  weights <- if (is.null(weights)) NULL else data[[weights]]
  idx <- resample_bootstrap_(seq_len(nrow(data)), weights = weights,
                             bayes = bayes)
  resample(data, idx)
}


#' @rdname resample_bootstrap
#' @importFrom assertthat is.string
#' @export
resample_bootstrap.grouped_df <- function(data,
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
  assert_that(is.flag(bayes))
  n <- nrow(data)
  g <- dplyr::n_groups(data)
  idx <- group_indices_lst(data)
  # fill in weights if none exist
  if (is.null(weights)) {
    weights <- rep(1, n)
  } else {
    weights <- data[[weights]]
  }
  # split weights by groups
  weights <- map(idx, ~ weights[.x])
  # resample the groups
  if (groups) {
    group_weights <-  if (weight_groups) {
      purrr::map_dbl(weights, sum)
    } else {
      rep(1, g)
    }
    grps <- resample_bootstrap_(seq_len(g), weights = group_weights,
                                bayes = bayes)
    idx <- idx[grps]
    weights <- weights[grps]
  }
  # resample within groups
  # within and these are not mutually exclusive
  if (stratify) {
    if (weight_within) {
      idx <- purrr::map2(idx, weights, function(x, w) {
        resample_bootstrap_(x, weights = w, bayes = bayes)
      })
    } else {
      idx <- map(idx, function(x) {
        resample_bootstrap(x, weights = NULL, bayes = bayes)
      })
    }
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

resample_bootstrap_ <- function(idx, weights = NULL, bayes = FALSE) {
  n <- length(idx)
  # resample weights
  if (bayes) {
    weights <- if (is.null(weights)) {
      1
    } else {
      (weights / sum(weights)) * n
    }
    weights <- stats::rgamma(length(idx), shape = weights)
  }
  sample(idx, size = n, replace = TRUE, prob = weights)
}
