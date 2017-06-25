#' Generate cross-validated time-series test/train sets
#'
#' Generate test/train set for time series. Each training set consists of
#' observations before the test set. This is also called "evaluation on a rolling
#' forecasting origin".
#'
#' @details In time-series cross-validation the training set only uses observations
#' that are prior to the test set. Suppose the time series has \eqn{n} observations,
#' the training set has a maximum size of \eqn{r \leq n}{r <= n} and minimum size of \eqn{s \geq r}{s >= r}.
#' and the test set has a maximum size of \eqn{p \leq n}{p <= n} and minimum size of \eqn{q \geq p}{q >= p}.
#' For indices \eqn{i \in \{1, \dots, N\}}:
#' \enumerate{
#' \item{Select observations \eqn{i, \dots, \max{p, n}} for the test set.}
#' \item{Select observations \eqn{\max{i - h - p}, \dots, i - h} for the training set.}
#' \item{If the test set has a size of at least \code{q} and the training set
#'       has a size of at least \code{r}.}
#' }
#'
#' @param data A data frame
#' @param horizon Difference between the first test set observation and the last training set observation
#' @param test_size Size of the test set
#' @param test_partial If \code{TRUE}, then allows for partial test sets. If \code{FALSE},
#'   does not allow for partial test sets.  If a number, then it is the minimum allowable
#'   size of a test set.
#' @param train_partial Same as \code{test_partial}, but for the training set.
#' @param train_size The maximum size of the training set. This allows for a rolling window
#'   training set, instead of using all observations from the start of the time series.
#' @param test_start,from,to,by An integer vector of the starting index values of the test set.
#'   \code{NULL}, then these are generated from \code{seq(from, to, by)}.
#' @param ... Arguments passed to methods
#' @references
#' \itemize{
#' \item{Hyndman RJ (2017). \emph{forecast: Forecasting functions for time series and linear models}. R package version 8.0, \href{http://github.com/robjhyndman/forecast}{URL}.}
#' \item{Hyndman RJ and Khandakar Y (2008). "Automatic time series forecasting: the forecast package for R." \emph{Journal of Statistical Software}. \href{http://www.jstatsoft.org/article/view/v027i03}{URL}.}
#' \item{Rob J. Hyndman. \href{Cross-validation for time series}{http://robjhyndman.com/hyndsight/tscv/}. December 5, 2016.}
#' \item{Rob J. Hyndman. \href{Time series cross-validation: an R example}{http://robjhyndman.com/hyndsight/tscvexample/}. August 26, 2011.}
#' \item{Rob J. Hyndman and George Athanasopoulos. "Evaluating Forecast Accuracy." \href{https://www.otexts.org/fpp/2/5}{URL}.}
#' \item{Max Kuhn. "Data splitting for Time Series." \emph{The caret Package}. 2016-11-29. \href{https://topepo.github.io/caret/data-splitting.html}{URL}.}
#' }
#' @export
crossv_ts <- function(data, ...) {
  UseMethod("crossv_ts")
}

#' @export
crossv_ts.data.frame <- function(data,
                                 horizon = 1L,
                                 test_size = 1L,
                                 test_partial = FALSE,
                                 train_partial = TRUE,
                                 train_size = NULL,
                                 test_start = NULL,
                                 from = 1L,
                                 to = NULL,
                                 by = 1L, ...) {
  n <- resample_idx_len(data)
  out <- crossv_ts_(n,
             horizon = horizon,
             test_size = test_size,
             test_partial = test_partial,
             train_partial = train_partial,
             train_size = train_size,
             test_start = test_start,
             from = from, to = to, by = by)
  for (i in c("train", "test")) {
    out[[i]] <- resample_lst(data, out[[i]], ...)
  }
  out
}

#' @export
crossv_ts.grouped_df <- function(data,
                                 horizon = 1L,
                                 test_size = 1L,
                                 test_partial = FALSE,
                                 train_partial = TRUE,
                                 train_size = n,
                                 test_start = NULL,
                                 from = 1L,
                                 to = n,
                                 by = 1L, ...) {
  n <- resample_idx_len(data, groups = TRUE)
  out <- crossv_ts_(n,
                    horizon = horizon,
                    test_size = test_size,
                    test_partial = test_partial,
                    train_partial = train_partial,
                    train_size = train_size,
                    test_start = test_start,
                    from = from, to = to, by = by)
  for (i in c("train", "test")) {
    out[[i]] <- resample_lst(data, out[[i]], groups = TRUE)
  }
  out
}

crossv_ts_1 <- function(test_start, test_end, train_start, train_end) {
  tibble(train = list(train_start:train_end),
         test = list(test_start:test_end),
         .id = as.integer(test_start))
}

#' @importFrom purrr pmap_df
#' @noRd
crossv_ts_ <- function(n,
                       horizon = 1L,
                       test_size = 1L,
                       test_partial = FALSE,
                       train_partial = TRUE,
                       train_size = n,
                       test_start = NULL,
                       from = 1L,
                       to = n,
                       by = 1L) {
  test_start <- test_start %||% seq(from, to, by)
  test_end <- pmin(test_start + test_size - 1L, n)
  test_len <- test_end - test_start + 1L
  train_end <- test_start - horizon
  train_start <- pmax(train_end - train_size + 1L, 1L)
  train_len <- train_end - train_start + 1L
  # check the validity of these segments
  keep_test <- (test_len >= test_size) |
      (test_partial & test_len >= test_partial)
  keep_train <- (train_len >= train_size) |
      (train_partial & train_len >= train_partial)
  idx <- keep_test & keep_train
  pmap_df(list(test_start = test_start[idx],
               test_end = test_end[idx],
               train_start = train_start[idx],
               train_end = train_end[idx]),
          crossv_ts_1)
}
