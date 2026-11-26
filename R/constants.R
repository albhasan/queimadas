#' Get satellites' names
#'
#' @description
#' Get a character vector with the names of the reference satellites used by
#' the Queimadas program.
#'
#' @return a character.
#'
#' @export
#'
get_satellites <- function() {
  sat_char <- c(
    "AQUA_M-T",
    "NOAA-12",
    "NOAA-20",
    "NOAA-21",
    "NPP-375-PM",
    "NPP-375D"
  )
  return(sat_char)
}

# TODO: Add satellites operational lives to figures. Perhaps a new funcion?
# https://space.oscar.wmo.int/satellites/view/aqua
# https://space.oscar.wmo.int/satellites/view/noaa_12
# https://space.oscar.wmo.int/satellites/view/terra
# https://space.oscar.wmo.int/satellites/view/snpp
# sat_metadata <- tibble::tribble(
#   ~name,     ~launch,      ~elo,
#   "AQUA",    "2002-05-04", ">2026",
#   "TERRA",   "1999-12-18", ">2026",
#   "NOAA-12", "1991-05-12", "2007-08-10",
#   "SNPP",    "2011-10-28", ">2029"
# )
