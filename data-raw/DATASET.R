library(dplyr)
library(geobr)
library(sf)

brstate_tb <-
  geobr::read_state(year = 2025) |>
  sf::st_drop_geometry() |>
  dplyr::select(
    code_state,
    abbrev_state,
    name_state,
    code_region,
    name_region
  )

grid_gpk <- path.expand(
  "~/Documents/data/r_packages/queimadas/grade_tm_util.gpkg"
)
stopifnot("Grid geopackage not found!" = file.exists(grid_gpk))

grid_sf <-
  grid_gpk |>
  sf::read_sf()

usethis::use_data(brstate_tb, grid_sf, overwrite = TRUE)



#---- Data for tests ----

lm_file <- path.expand(
  "~/Documents/results/r_packages/queimadas/cell_lm/active-fires_233_067_lm.RDS"
)
ts_file <- path.expand(
  "~/Documents/results/r_packages/queimadas/cell_ts/active-fires_233_067.RDS"
)

test_lm <- readRDS(lm_file)
test_ts <- readRDS(ts_file)

usethis::use_data(test_ts, test_lm, internal = TRUE, overwrite = TRUE)
