#' Leave-p-out cross-validation
#'
#' Leave-one-out and leave-p-out cross-validation.
#'
#' @param data A data frame
#' @param p Number of observations or groups to leave out
#' @param ... Arguments passed to methods
#' @return A data frame with n rows and columns \code{test} and \code{train}.
#'   \code{test} and \code{train} are list-columns containing
#'   \code{\link{resample}} objects.
#' @importFrom modelr crossv_kfold
#' @export
crossv_loo <- function(data, ...) {
  UseMethod("crossv_loo")
}

#' @describeIn crossv_loo Leave-one-out cross validation
#' @export
crossv_loo.data.frame <- function(data, ...) {
  idx <- seq_len(nrow(data))
  tibble(
    train = map(idx, function(i) resample(data, setdiff(idx, i))),
    test = map(idx, function(i) resample(data, i)),
    .id = id(nrow(data))
  )
}

#' @describeIn crossv_loo Leave-one-group out cross validation
#' @export
crossv_loo.grouped_df <- function(data, ...) {
  idx <- group_indices_lst(data)
  grps <- seq_along(idx)
  tibble(
    train = map(grps, function(g) {
      resample(data, flatten_int(idx[setdiff(grps, g)]))
    }),
    test = map(grps, function(g) {
      resample(data, idx[[g]])
    }),
    .id = id(length(grps))
  )
}

#' @rdname crossv_loo
#' @export
crossv_lpo <- function(data, p, ...) {
  UseMethod("crossv_lpo")
}

# stratified lpo would require equal sized groups

#' @describeIn crossv_loo Leave-p-out out cross validation
#' @export
crossv_lpo.data.frame <- function(data, p = 1, ...) {
  assert_that(is.number(p) && p >= 0)
  p <- as.integer(p)
  idx <- seq_len(nrow(data))
  test_idx <- utils::combn(idx, p, simplify = FALSE)
  tibble(
    train = map(test_idx, function(i) resample(data, setdiff(idx, i))),
    test = map(test_idx, function(i) resample(data, i)),
    .id = id(length(test_idx))
  )
}

#' @describeIn crossv_loo Leave-p-groups out cross validation
#' @export
crossv_lpo.grouped_df <- function(data, p = 1, ...) {
  assert_that(is.number(p) && p >= 0)
  p <- as.integer(p)
  idx <- seq_len(nrow(data))
  test_idx <- map(utils::combn(group_indices_lst(data), p, simplify = FALSE),
                  flatten_int)
  tibble(
    train = map(test_idx, function(i) resample(data, setdiff(idx, i))),
    test = map(test_idx, function(i) resample(data, i)),
    .id = id(length(test_idx))
  )
}
