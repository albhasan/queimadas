#' Utilitary function to get a column from a data frame
#'
#' @description
#' Extract the data from a a column in a data frame.
#'
#' @param x a data frame.
#' @param cname a character. Name of an object (column or element) in `x`.
#'
#' @return a vector.
#'
#' @export
#'
get_cdata <- function(x, cname) {
  stopifnot("Name not found in data frame!" = all(cname %in% names(x)))
  return(x[[cname]])
}
