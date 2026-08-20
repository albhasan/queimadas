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

#' Get an aggregated bottom row
#'
#' @description
#' Get a row with the results of applying a funcion to each numeric column of
#' the given data frame.
#'
#' @param data_tb a data frame.
#' @param f a function.
#' @param na_rm a logical. A parameter passed to `f`.
#'
#' @return an one-row data frame.
#'
#' @export
#'
get_bottom_row <- function(data_tb, f, na_rm = FALSE) {
  data_tb |>
    dplyr::summarize(
      tibble::tibble(
        dplyr::across(tidyselect::where(is.numeric), ~ f(.x, na.rm = na_rm)),
      )
    )
}
