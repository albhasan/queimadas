#' Get fire data from Brazil
#'
#' @description
#' Get data from the database coresponding to Brazil, excluding industrial
#' areas.
#'
#' @param db_con a conection to a data base of queimadas.
#' @param table_name name of the table in the data base.
#'
#' @return a DBI object. This object requires a call to `dplyr::collect()` to
#' retrieve the actual data.
#'
#' @importFrom magrittr "%>%"
#'
#' @export
#'
get_brazil <- function(db_con, table_name) {
  #TODO: Remove magrittr dependency!
  id_area_industrial <- data_pas <- pais <- NULL
  db_con %>%
  dplyr::tbl(table_name) %>%
  dplyr::filter(
    pais == "Brasil",
    id_area_industrial == "0"
  ) %>%
  dplyr::mutate(
    afternoon = lubridate::hour(lubridate::as_datetime(data_pas)) >= 12
  ) %>%
  return()
}



#' Get fire data from Brazil by year and month
#'
#' @description
#' Get data from the database coresponding to Brazil, excluding industrial
#' areas, aggregated by year and month.
#'
#' @param db_con a conection to a data base of queimadas.
#' @param table_name name of the table in the data base.
#'
#' @return a DBI object. This object requires a call to `dplyr::collect()` to
#' retrieve the actual data.
#'
#' @importFrom magrittr "%>%"
#'
#' @export
#'
get_brazil_year_month <- function(db_con, table_name) {
  #TODO: Remove magrittr dependency!
  afternoon <- data_pas <- satelite <- period <- NULL
  db_con %>%
  get_brazil(table_name = table_name) %>%
   dplyr::mutate(
     period = stringr::str_sub(string = data_pas, start = 1L, end = 7L),
     satelite = dplyr::if_else(
       condition = satelite == "NPP-375" & afternoon == 1,
       true = "NPP-375-PM",
       false = satelite
     ),
     satelite = dplyr::if_else(
       condition = satelite == "NPP-375" & afternoon == 0,
       true = "NPP-375-AM",
       false = satelite
     )
   ) %>%
  dplyr::select(satelite, period) %>%
  dplyr::summarize(
    n = dplyr::n(),
    .by = tidyselect::all_of(x = c("period", "satelite"))
  ) %>%
  dplyr::arrange(period, satelite) %>%
  return()
}



#' Get fire data from Brazil by month
#'
#' @description
#' Get data from the database coresponding to Brazil, excluding industrial
#' areas, aggregated by month (all years).
#'
#' @param db_con a conection to a data base of queimadas.
#' @param table_name name of the table in the data base.
#'
#' @return a DBI object. This object requires a call to `dplyr::collect()` to
#' retrieve the actual data.
#'
#' @importFrom magrittr "%>%"
#'
#' @export
#'
get_brazil_month <- function(db_con, table_name) {
  #TODO: Remove magrittr dependency!
  n <- period <- NULL
  #brazil_m_tb <-
  #brazil_ym_tb %>%
  db_con %>%
  get_brazil_year_month (table_name = table_name) %>%
  dplyr::mutate(
      period = stringr::str_sub(string = period, start = 6L, end = 7L)
  ) %>%
  dplyr::summarize(
    n = sum(n, na.rm = FALSE),
    .by = tidyselect::all_of(x = c("period", "satelite"))
  ) %>% 
  return()
}



#' Get pairs of satellites for analysis
#'
#' @description
#' Get a data frame of pairs of satellites for analysis.
#'
#' @param data_tb a tibble with data from queimadas.
#' @param satellites a character with satellite names to include. Among their
#' names, it must be those of candidate satelltes.
#'
#' @return a tibble.
#'
#' @importFrom magrittr "%>%"
#'
#' @export
#'
get_sat_pairs <- function(data_tb, satellites) {
  stopifnot("Candidate names not found in satellites!" =
            "candidate" %in% names(satellites))

  satelite <- satelite_x <- satelite_y <- NULL
  . <- NULL

  #TODO: Remove magrittr dependency!
  data_tb %>%
  dplyr::select(satelite) %>%
  dplyr::distinct(satelite) %>%
  dplyr::filter(satelite %in% satellites) %>%
  dplyr::pull(satelite) %>%
  tidyr::expand_grid(satelite_x = ., satelite_y = .) %>%
  dplyr::filter(
    satelite_x != satelite_y,
    satelite_y %in% satellites[names(satellites) == "candidate"]
  ) %>%
  dplyr::arrange(satelite_y) %>%
  return()
}
