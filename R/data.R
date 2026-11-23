#' Brazilian states
#'
#' Brazilian states (2025) as vectors (`sf` object).
#'
#' @format ## `brstate_sf`
#' A data frame with 27 rows and 5 columns:
#' \describe{
#'   \item{code_state}{State ID}
#'   \item{abbrev_state}{State name abbreviated to 2 letters.}
#'   \item{name_state}{State name}
#'   \item{code_region}{Code of the region to which the state belongs}
#'   \item{name_region}{Name of the region}
#'   \item{geometry}{Column with vector data}
#' }
#' @source <https://cran.r-project.org/package=geobr>
"brstate_sf"

#' Metadata from Brazilian states
#'
#' Metadata from Brazilian states (2025) which could provide additional levels of
#' aggregation.
#'
#' @format ## `brstate_tb`
#' A data frame with 27 rows and 5 columns:
#' \describe{
#'   \item{code_state}{State ID}
#'   \item{abbrev_state}{State name abbreviated to 2 letters.}
#'   \item{name_state}{State name}
#'   \item{code_region}{Code of the region to which the state belongs}
#'   \item{name_region}{Name of the region}
#' }
#' @source <https://cran.r-project.org/package=geobr>
"brstate_tb"



#' Brazilian biomes
#'
#' Brazilian biomes (2025) as vectors (`sf` object).
#'
#' @format ## `brstate_sf`
#' A data frame with 27 rows and 5 columns:
#' \describe{
#'   \item{code_biome}{Biome ID}
#'   \item{name_biome}{Biome name}
#'   \item{year}{Year of the vectors.}
#'   \item{geometry}{Column with vector data}
#' }
#' @source <https://cran.r-project.org/package=geobr>
"biomes_sf"



#' Grid used for spatial aggregation
#'
#' This is the reference grid used for the spatial aggregation of data during
#' analysis.
#'
#' @format ## `grid_sf`
#' A data frame with 988 rows and 6 columns:
#' \describe{
#'   \item{id}{Identifier.}
#'   \item{gid}{Geometry identifier}
#'   \item{path_row}{Identifier build using the relative location of the cell in the grid.}
#'   \item{orbita}{Path as an integer.}
#'   \item{ponto}{Row as an integer.}
#'   \item{geom}{Geometry.}
#' }
#'
"grid_sf"
