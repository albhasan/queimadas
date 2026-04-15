library(dplyr)
library(geobr)
library(sf)

brstate_tb <-
  geobr::read_state() %>%
  sf::st_drop_geometry()

grid_gpk <- "/home/alber/Documents/data/r_packages/queimadas/grade_tm_util.gpkg"
stopifnot("Grid geopackage not found!" = file.exists(grid_gpk))

grid_sf <-
  grid_gpk |>
  sf::read_sf()

usethis::use_data(brstate_tb, grid_sf, overwrite = TRUE)
