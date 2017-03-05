#' @importFrom modelr resample
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map flatten_int transpose
NULL

id <- function(n) {
  width <- nchar(n)
  sprintf(paste0("%0", width, "d"), seq_len(n))
}

group_indices_lst <- function(data) {
  g <- seq_len(dplyr::n_groups(data))
  split(dplyr::group_indices(data), g)
}

# Return either row numbers or group numbers
data_idx <- function(data) {
  UseMethod("data_idx")
}

data_idx.data.frame <- function(data) {
  seq_len(nrow(data))
}

data_idx.grouped_df <- function(data) {
  seq_len(dplyr::n_groups(data))
}
