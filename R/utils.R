#' @importFrom modelr resample
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map flatten_int transpose %||%
#' @importFrom assertthat assert_that is.number is.flag
NULL

id <- function(n) {
  width <- nchar(n)
  sprintf(paste0("%0", width, "d"), seq_len(n))
}

group_indices_lst <- function(data) {
  g <- dplyr::group_indices(data)
  split(seq_along(g), g)
}
