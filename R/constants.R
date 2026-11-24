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
