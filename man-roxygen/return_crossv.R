#' @return A data frame with <%= numrows %> rows and the following columns:
#' \describe{
#' \item{train}{A list column with objects representing the training sets. For \code{<%= f %>}, the elements are vectors of the indexes; for \code{<%= fn %>}, the elements are \code{\link[=LazySample]{lazy_sample}} objects.}
#' \item{test}{A list column with objects representing the training sets. For \code{<%= f %>}, the elements are vectors of the indexes; for \code{<%= fn %>}, the elements are \code{\link[=LazySample]{lazy_sample}} objects.}
#' }
