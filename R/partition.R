#' Generate a partition of data
#'
#'
#'
#'
# resample_partition <- function(data, p, ...) {
#   UseMethod("resample")
# }
#
# resample_partition.data.frame <- function(data, p, ...) {
#   modelr::resample_partition(data, p)
# }
#
# x <- partition_groups(data, p) {
#
# }
#
# resample_partition.grouped_df <- function(data, p, ...) {
#
# }
#
# partition_p <- function(n, p) {
#   if (!is.numeric(p) || length(p) < 2 || !all(has_name(p))) {
#     stop("`p` must be a named numeric vector with at least two values.")
#   }
#   if (abs(sum(p) - 1) > 1e-06) {
#     message("Rescaling `p` to sum to 1.")
#   }
#   p <- p/sum(p)
#   g <- findInterval(seq_len(n)/n, c(0, cumsum(p)), rightmost.closed = TRUE)
#   idx <- split(seq_len(n), sample(g))
#   names(idx) <- names(p)
# }
#
# partition_k <- function(n, k) {
#   rep(ceiling(n / k)
# }