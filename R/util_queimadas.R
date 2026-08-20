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

#' Format a forecast to look like Queimadas web table
#'
#' @description
#' Given a forecast data frame, format it to look like the the tables used in
#' the Queimadas web page.
#'
#' @param data_tb a data frame representing a Queimadas forecast.
#'
#' @return a data frame where each year correspond to a row and each column to
#' a month.
#'
#' @export
#'
get_queimadas_web_table <- function(data_tb) {
  f <- fit <- month <- period <- r <- year <- NULL

  stopifnot(
    "Missing columns in forecast data frame!" =
      c("period", "fit") %in% colnames(data_tb)
  )

  # Split the period into year and mont and then organize years into rows
  # months into columns.
  yxm_tb <-
    data_tb |>
    dplyr::select(period, fit) |>
    tidyr::separate(
      col = period,
      into = c("year", "month"),
      sep = "-"
    ) |>
    dplyr::mutate(
      month = stringr::str_c("m", month)
    ) |>
    tidyr::pivot_wider(
      id_cols = year,
      names_from = month,
      values_from = fit
    ) |>
    dplyr::select(year, sort(tidyselect::peek_vars()))

  # Add the total of each year (rows).
  yxm_yt_tb <-
    yxm_tb |>
    dplyr::mutate(
      total = rowSums(
        dplyr::across(tidyselect::where(is.numeric)),
        na.rm = TRUE
      )
    )

  # Get the column totals (month and year total).
  ct_tb <-
    tibble::tribble(
      ~fname, ~f,
      "max", max,
      "mean", mean,
      "min", min
    ) |>
    dplyr::mutate(
      r = purrr::map(
        .x = f,
        .f = function(f, data_tb) {
          get_bottom_row(
            data_tb = data_tb,
            f = f,
            na_rm = TRUE
          )
        },
        data_tb = yxm_yt_tb
      )
    ) |>
    dplyr::select(-f) |>
    tidyr::unnest(r)

  # Merge the data and the bottom columns.
  res <-
    yxm_yt_tb |>
    dplyr::bind_rows(
      dplyr::rename(ct_tb, year = "fname")
    )

  return(res)
}

#' Get a LaTeX table code from a Queimadas web styled table
#'
#' @description
#' Take a data frame in the Queimadas web style and convert it the LaTeX code of
#' a simililar table.
#'
#' @param data_tb a data frame in the Queimadas style. Possibly resulting from
#' a call to `get_queimadas_web_table()`.
#' @param tab_label a character(1). Label for the LaTeX's reference.
#' @param caption a character(1).
#'
#' @return a character.
#'
#' @export
#'
get_queimadas_web_table_latex <- function(data_tb, tab_label, caption) {
  # Get the positions of the min and maximum values for each column.
  Year <- NULL

  # Format character into camel case.
  # Adapted from
  # https://stackoverflow.com/questions/11672050/how-to-convert-not-camel-case-to-camelcase-in-r
  camel <- function(x) {
    capit <- function(x) {
      paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
    }
    return(
      sapply(
        X = strsplit(x, "\\."),
        FUN = function(x) paste(capit(x), collapse = "")
      )
    )
  }

  # Convert data frame to character.
  table_tex <-
    data_tb |>
    janitor::clean_names(case = "title") |>
    # NOTE: janitor::clean_names changes from "year" to "Year".
    dplyr::mutate(
      Year = camel(Year)
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        ~ round(.x, digits = 1)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        as.character
      )
    ) |>
    dplyr::mutate(dplyr::across(
      tidyselect::everything(),
      ~ replace(., is.na(.), "")
    ))

  # Get, for each column, the positions of the minimum and maximum values.
  pos_min <- c(NA, apply(X = data_tb[-1], FUN = which.min, MARGIN = 2))
  pos_max <- c(NA, apply(X = data_tb[-1], FUN = which.max, MARGIN = 2))
  stopifnot(length(pos_min) == ncol(data_tb))
  stopifnot(length(pos_max) == ncol(data_tb))

  # Color the maximum and mininum values by column.
  for (i in seq_along(pos_min)) {
    if (i == 1) next()
    table_tex[[pos_min[i], i]] <- kableExtra::cell_spec(
      table_tex[[pos_min[i], i]],
      format = "latex",
      background = "green"
    )
    table_tex[[pos_max[i], i]] <- kableExtra::cell_spec(
      table_tex[[pos_max[i], i]],
      format = "latex",
      background = "red"
    )
  }

  # Write to LaTeX's table using the Queimadas' webpage format.
  res <-
    table_tex |>
    kableExtra::kbl(
      digits = 1,
      row.names = FALSE,
      escape = FALSE,
      format = "latex",
      booktabs = TRUE,
      longtable = TRUE,
      linesep = "",
      align = "crrrrrrrrrrrrrr",
      caption = caption,
      label = tab_label
      # col.names = lapply(X = seq_along(.), FUN = function(x) {
      #   names <- paste("Var", x, sep = " ")
      # })
    ) |>
    kableExtra::kable_styling(
      latex_options = c("repeat_header", "hold_position")
    ) |>
    kableExtra::row_spec(
      row = nrow(data_tb) - 3,
      hline_after = TRUE
    ) |>
    kableExtra::row_spec(
      row = nrow(data_tb),
      background = "green"
    ) |>
    kableExtra::row_spec(
      row = nrow(data_tb) - 2,
      background = "red"
    )

  return(res)
}
