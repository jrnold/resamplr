#' @param min_len Minimum length for fitting
#' @param test_len Test data set length
#' @param test_start Test start time
#'
#' #' @references Rob Hyndman. Time series cross-validation: an R example. \url{http://robjhyndman.com/hyndsight/tscvexample/}
#' resample_crossv_ts <- function(data, i,
#'                                train_len = NULL,
#'                                train_minlen = NULL,
#'                                test_start = 1L,
#'                                test_minlen = 1L,
#'                                test_len = NULL) {
#'   test_minlen <- test_len %||% test_minlen
#'   train_start <- if (is.null(train)) {
#'     1L
#'   } else {
#'     i - test_len + 1L
#'   }
#'   train_idx <- seq.int(test_start, i, by = 1L)
#'   test_len <- test_len %||% nrow(data)
#'   test_idx <- seq.int(i + test_start,
#'                       data_frame(train = resample(data, test_idx),
#'                                  test = resample(data, test_idx)))
#' }
#'
#'
#' crossv_ts <- function(data,
#'                       train_len = NULL,
#'                       train_minlen = NULL,
#'                       test_start = 1L,
#'                       test_minlen = 1L,
#'                       test_len = NULL) {
#'
#' }
NULL