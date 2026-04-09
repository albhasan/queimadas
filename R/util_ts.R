#' Get the yearly breaklines for plot
#'
#' @description
#' Get the points to insert breaklines in for the figures (plots).
#'
#' @param data_tb a tibble representing a time series.
#' @param period_col a character. Name of the column identifying the time
#' period.
#' @param break_pattern a character to identify the break points.
#'
#' @return a character.
#'
#' @export
#'
get_break_lines_year <- function(data_tb, period_col = "period",
                                 break_pattern = "-08$") {
  period <- NULL
  stopifnot("Period column not found!" = period_col %in% colnames(data_tb))

  res <-
    data_tb |>
    dplyr::select(period) |>
    dplyr::distinct(period) |>
    dplyr::filter(
      stringr::str_detect(
        string = period,
        pattern = break_pattern
      )
    ) |>
    dplyr::pull(period)

  return(res)
}

#' Filter complete series
#'
#' @description
#' Keep those series with the complete number observations for a period.
#'
#' @param x a tibble with the variables period, satelite, and n.
#' @param n_obs number of observations in a period.
#'
#' @return a modified version of x.
#'
#' @export
#'
filter_complete_series <- function(x, n_obs) {
  stopifnot(
    "Missing columns in input data frame!" =
      c("satelite_x", "satelite_y") %in% colnames(x)
  )
  n_period <- satelite_x <- satelite_y <- sat_key <- NULL

  # Find satellits' combinations with 12 observations.
  complete_series <-
    x |>
    dplyr::summarize(
      n_period = dplyr::n(),
      .by = tidyselect::all_of(c("satelite_x", "satelite_y"))
    ) |>
    dplyr::filter(n_period >= n_obs) |>
    dplyr::select(-n_period) |>
    dplyr::mutate(
      sat_key = stringr::str_c(satelite_x, satelite_y, sep = "-")
    ) |>
    dplyr::select(sat_key)

  # Filter only series with at least 12 observations.
  res <-
    x |>
    dplyr::mutate(
      sat_key = stringr::str_c(satelite_x, satelite_y, sep = "-")
    ) |>
    dplyr::right_join(
      y = complete_series,
      by = "sat_key"
    ) |>
    dplyr::select(-sat_key)

  return(res)
}

#' Add the family to which the satelite belongs to
#'
#' @param x a tibble with time series of observations.
#'
#' @return the tibble `x` with a new column called `sat_family`.
#'
#' @export
#'
add_sat_family <- function(x) {
  res <-
    x |>
    dplyr::mutate(
      sat_family = dplyr::case_when(
        stringr::str_detect(string = satelite, pattern = "AQUA") ~ "AQUA",
        stringr::str_detect(string = satelite, pattern = "GOES") ~ "GOES",
        stringr::str_detect(string = satelite, pattern = "NOAA") ~ "NOAA",
        stringr::str_detect(string = satelite, pattern = "NPP") ~ "NPP",
        stringr::str_detect(string = satelite, pattern = "TERRA") ~ "TERRA",
        .default = "OTHER"
      )
    )

  return(res)
}
