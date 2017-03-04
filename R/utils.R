#' @importFrom modelr resample
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map map_int map_lgl map_df flatten_int %||% transpose
NULL

resample <- function(data, idx) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (is.numeric(idx)) {
    idx <- as.integer(idx)
  }
  if (!is.integer(idx)) {
    stop("`idx` must be an integer vector.")
  } else if (any(is.na(idx))) {
    stop("All elements of `idx` must be non-missing integers.")
  } else if (any(idx < 1) || any(idx > nrow(data))) {
    stop("All elements of `idx` must be between 1 and `nrow(data)`.")
  }
  structure(list(data = data, idx = idx), class = "resample")
}

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

# split indexes into k groups
split_kfold <- function(idx, k, shuffle = TRUE) {
  n <- length(idx)
  folds <- if (shuffle) {
    sample(rep(seq_len(k), length.out = n), n, replace = FALSE)
  } else {
    cut(x, k, include.lowest = TRUE, labels = FALSE)
  }
  split(idx, folds)
}


# group ids
group_ids <- function(data) {
  seq_len(dplyr::n_groups(data))
}

resample_list <- function(data, idxs) {
  map(idxs, resample, data = data)
}

bs_ts <- function(n, m, size = 1, sim = "fixed", endcorr = FALSE) {
  endpt <- if (endcorr) {
    n
  } else {
    n - size + 1
  }
  if (sim == "geom") {
    len_tot <- 0
    lens <- NULL
    while (len_tot < m) {
      temp <- 1 + stats::rgeom(1, 1 / size)
      temp <- pmin(temp, m - len_tot)
      lens <- c(lens, temp)
      len_tot <- len_tot + temp
    }
    st <- sample.int(endpt, length(lens), replace = TRUE)
  } else {
    nn <- ceiling(m / size)
    lens <- c(rep(size, nn - 1), 1 + (m - 1) %% size)
    st <- sample.int(endpt, nn, replace = TRUE)
  }
  purrr::map2_int(st, lens, function(s, sz) {
    if (sz > 1) seq(s, s + sz - 1L)
    else integer()
  })
}

