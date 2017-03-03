#' @importFrom modelr resample
#' @importFrom tibble tibble
#' @importFrom purrr map map_int map_lgl map_df flatten_int %||% transpose
NULL

id <- function(n) {
  width <- nchar(n)
  sprintf(paste0("%0", width, "d"), seq_len(n))
}

#' Is it a resample object?
#'
#' Checks whether an object is an \code{\link[modelr]{resample}}
#' object.
#'
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
is.resample <- function(x) inherits(x, "resample")

#' @export
c.resample <- function(...) {
  objs <- list(...)
  if (length(objs) == 1) return(objs[[1]])
  if (!all(map_lgl(objs, is.resample))) {
    stop("All objects must inherit from class `resample`.",
         call. = FALSE)
  }
  identical_data <- function(x) identical(x[["data"]], objs[[1]][["data"]])
  if (!all(map_lgl(objs[-1], identical_data))) {
    stop("All resample objects must have identical data", call. = FALSE)
  }
  resample(objs[[1]][["data"]],
           flatten_int(map(objs, as.integer)))
}


# extract indexes of a numbered group from a grouped_df
get_group_indexes <- function(data, groups = NULL) {
  # indices are 0-indexed
  idx <- map(attr(data, "indices"), `+`, 1L)
  if (!is.null(groups)) idx <- idx[groups]
  idx
}

get_group_indexes_int <- function(data, groups = NULL) {
  flatten_int(get_group_indexes(data, groups = groups))
}

# split indexes by groups from a grouped_df
split_idx_by_group <- function(data, ids) {
  idx <- get_group_indexes_int(data, ids)
  list(as.integer(idx), as.integer(setdiff(seq_len(nrow(data)), idx)))
}

rpartition <- function(x, k) {
  n <- length(x)
  parts <- sample(rep(seq_len(k), length.out = n), n, replace = FALSE)
  split(x, parts)
}

group_ids <- function(data) seq_len(dplyr::n_groups(data))

#' Create a list of resample objects
#'
#' Create a list of resample objects from a data frame and a list
#' of indexes.
#'
#' @param data A data frame
#' @param idx A list of integer vectors of indexes
#' @return A list of \code{\link[modelr]{resample}} objects.
resample_list <- function(data, idxs) {
  map(idxs, function(i) resample(data, as.integer(i)))
}

# map_resample_df <- function(data, idx) {
#   tibble::as_tibble(transpose(map(idx, function(i, data) resample(data, i),
#                                   data = data)))
# }


sample_tsboot <- function(n, m, size = 1, sim = "fixed", endcorr = FALSE) {
  endpt <- if (endcorr) {
    n
  } else {
    n - size + 1
  }
  if (sim == "geom") {
    len_tot <- 0
    lens <- NULL
    while (len_tot < m) {
      temp <- 1 + rgeom(1, 1 / size)
      temp <- pmin(temp, m - len_tot)
      lens <- c(lens, temp)
      len_tot <- len_tot + temp
    }
    st <- sampl.int(endpt, length(lens), replace = TRUE)
  } else {
    nn <- ceiling(m / size)
    lens <- c(rep(size, nn - 1), 1 + (m - 1) %% size)
    st <- sample.int(endpt, nn, replace = TRUE)
  }
  map2_int(st, lens, function(s, sz) {
    if (sz > 1) seq(s, s + sz - 1L)
    else integer()
  })
}

sample_tskfold <- function(idx, k) {
  g <- cut(idx, k, include.lowest = TRUE, labels = FALSE)
  idx_list <- split(idx, g)
  map(seq_len(k), function(i) {
    list(train = idx_list[1:i], test = idx_list[(i + 1):k])
  })
}