library(DBI)
library(dplyr)
library(purrr)
library(sf)
library(stringr)
library(tibble)
library(tools)

library(queimadas)

#---- Configuration ----

gpkg_dir <- "/home/alber/Documents/data/r_packages/queimadas/results/gpkg"
stopifnot("Directory with GeoPackage data not found!" = dir.exists(gpkg_dir))

sqlite_file <- "/home/alber/Documents/data/r_packages/queimadas/fire_grid.sqlite"
if(file.exists(sqlite_file)) {
  stop("Database file already exists!")
}
table_name = "fire_foci_grid"

#---- Get data from the database files ----

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
    data_sf = purrr::map(
      .x = gpkg_file,
      .f = sf::read_sf
    )
  ) |>
  dplyr::mutate(
    data_tb = purrr::map(
      .x = data_sf,
      .f = sf::st_drop_geometry
    )
  ) |>
  dplyr::select(data_tb) |>
  tidyr::unnest(data_tb)

# Write data to a SQLite database.
db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)
if (DBI::dbExistsTable(db_con, table_name)) {
  stop(sprintf("The table %s already exists!", table_name))
} else {
  message(sprintf("Creating the table %s ...", table_name))
}

tryCatch({
  message("Loading data to the database...")
  DBI::dbWriteTable(
    conn = db_con,
    name = table_name, 
    value = data_tb,
    overwrite = FALSE,
    append = TRUE
  )
},
error = function(e) {
  message("Error while loading data to the database")
},
finally = {
  message("Closing database's connection...")
  DBI::dbDisconnect(db_con)
  gc()
})
