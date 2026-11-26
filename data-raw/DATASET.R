library(dplyr)
library(geobr)
library(sf)

geobr_year <- 2025

brstate_sf <-
  geobr::read_state(
    year = geobr_year,
    simplified = TRUE,
    output = "sf",
    showProgress = FALSE,
    cache = TRUE,
    verbose = FALSE
  ) |>
  dplyr::select(
    code_state,
    abbrev_state,
    name_state,
    code_region,
    name_region
  )

brstate_tb <-
  brstate_sf |>
  sf::st_drop_geometry()

biomes_sf <-
  geobr::read_biomes(
    year = geobr_year,
    simplified = TRUE
  )

grid_gpk <- path.expand(
  "~/Documents/github/queimadas/inst/extdata/data/grade_tm_util.gpkg"
)
stopifnot("Grid geopackage not found!" = file.exists(grid_gpk))

grid_sf <-
  grid_gpk |>
  sf::read_sf()

usethis::use_data(biomes_sf, brstate_sf, brstate_tb, grid_sf, overwrite = TRUE)



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
