#' find ouliers
#'
#' @description
#' Find outliers in the given data.
#'
#' @param x a numeric.
#'
#' @return a numeric.
#'
#' @export
#'
find_outliers <- function(x) {
  grDevices::boxplot.stats(x = x, coef = 1.5, do.conf = TRUE, do.out = TRUE)$out
}
