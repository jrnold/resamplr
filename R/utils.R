id <- function(n) {
  width <- nchar(n)
  sprintf(paste0("%0", width, "d"), seq_len(n))
}

is.resample <- function(x) inherits(x, "resample")

c.resample <- function(...) {
  objs <- list(...)
  if (length(objs) == 1) return(objs)
  if (!all(purrr::map_lgl(objs, is.resample))) {
    stop("All objects must inherit from class ", sQuote("resample"), ".",
         call. = FALSE)
  }
  identical_data <- function(x) identical(x[["data"]], objs[[1]][["data"]])
  if (!all(purrr::map_lgl(objs[-1], identical_data))) {
    stop("All resample objects must have identical data", call. = FALSE)
  }
  modelr::resample(objs[[1]][["data"]],
                   purrr::flatten_int(map(objs, as.integer)))
}
