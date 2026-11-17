library(DBI)
library(dplyr)
library(purrr)
library(logger)
library(sf)
library(stringr)
library(tibble)
library(tools)

library(queimadas)



#---- Configuration ----

logger::log_info("Reading configuration...")

gpkg_dir <- "/home/alber/Documents/data/r_packages/queimadas/results/gpkg"
stopifnot("Directory with GeoPackage data not found!" = dir.exists(gpkg_dir))

sqlite_file <- 
  "/home/alber/Documents/data/r_packages/queimadas/fire_grid.sqlite"
if(file.exists(sqlite_file)) {
  stop("Database file already exists!")
}
table_name = "fire_foci_grid"



#---- Get data from the database files ----

logger::log_info(
  "Export data from geopackage files into a non-spatial database..."
)

data_tb <- 
  gpkg_dir |>
  list.files(
    pattern = "*.gpkg$",
    full.names = TRUE
  ) |>
  tibble::as_tibble() |>
  dplyr::rename(gpkg_file = "value") |>
  dplyr::mutate(
    filename = basename(gpkg_file),
    filename = tools::file_path_sans_ext(filename),
    year = stringr::str_extract(
      string = filename,
      pattern = "[0-9]{4}"
    )
  ) |>
  dplyr::mutate(
    gpgk2 = purrr::map(
      .x = gpkg_file,
      .f =  sf_data_to_db,
      sqlite_file = sqlite_file,
      table_name = table_name
    )
  )

logger::log_info("Finishec!")
