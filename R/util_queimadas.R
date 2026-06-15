#' Get the dates of usage of the reference satellites
#'
#' @description
#' This function returns the time interval in which the Queimadas program used
#' each of the reference satellites.
#'
#' @returns a data frame (tibble) with data about the time intervals of the
#' reference satellites in the Queimadas program:
#'
#' * id an integer(1).
#' * satellite a character(1). Satellite's name.
#' * sensor a character(1). Name of the sensor on board of the satellite.
#' * launch_date a Date(1) object.
#' * from,to a Date(1). Date from which the reference satellite is (or was)
#' used as reference satellite by the Queimadas program.
#'
#' @seealso
#' Queimadas program, frequently asked questions, questions 7, 38, and 39
#' `https://terrabrasilis.dpi.inpe.br/queimadas/portal/pages/secao_informacoes/faq/index.html`
#'
#' @export
#'
get_queimadas_dates <- function() {
  from <- to <- NULL
  res <-
    tibble::tribble(
      ~id, ~satellite, ~sensor, ~launch_date, ~from, ~to,
      1L, "NOAA-12", "AVHRR", "1991-05-14", "1998-06-01", "2002-07-03",
      2L, "AQUA_M-T", "MODIS", "2002-05-04", "2002-07-07", NA,
      3L, "SUOMI-NPP", "VIIRS", "2011-10-28", NA, NA,
    ) |>
    dplyr::mutate(
      from = lubridate::as_date(from),
      to = lubridate::as_date(to)
    )

  return(res)
}

#' Get the MODIS collections
#'
#' @description
#' This function returns a data frame (tibble) with data regarding MODIS
#' collections.
#'
#' @returns a data frame (tibble) with the following columns:
#'
#' * id an integer.
#' * satellite a character(1). Satellite (or platform) name.
#' * sensor a character(1). Sensor's name.
#' * collection a character(1). MODIS collection number.
#' * from,to a Date(1) object. The dates of start and end of temporal coverage
#' of the collection.
#'
#' @seealso
#' Summary of the differences between various versions of MODIS data
#' `https://nsidc.org/data/modis/version-history`.
#'
#' @export
#'
get_modis_collections_dates <- function() {
  from <- to <- NULL
  res <-
    tibble::tribble(
      ~id, ~satellite, ~sensor, ~collection, ~from, ~to,
      1L, "AQUA", "MODIS", "V61", "2002-07-04", NA,
      2L, "TERRA", "MODIS", "V61", "2000-02-24", NA,
      3L, "AQUA", "MODIS", "V6", "2002-07-04", "2023-02-25",
      4L, "TERRA", "MODIS", "V6", "2000-02-24", "2022-02-17",
      5L, "AQUA", "MODIS", "V5", "2002-07-04", "2017-01-02",
      6L, "TERRA", "MODIS", "V5", "2000-02-24", "2017-01-02",
      7L, "AQUA", "MODIS", "V4", "2002-07-04", "2007-01-03",
      8L, "TERRA", "MODIS", "V4", "2000-02-24", "2007-01-03",
    ) |>
    dplyr::mutate(
      from = lubridate::as_date(from),
      to = lubridate::as_date(to)
    )
  return(res)
}
