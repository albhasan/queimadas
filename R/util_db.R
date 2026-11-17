#' Load data (no geometry) to a database.
#'
#' @description
#' Utilitary function. Given a file to a geopackge, read it, remove its
#' geometry, and load it to a table in a SQLite database.
#'
#' @param gpkg_file a character(1). Path to a geopackage file.
#' @param sqlite_file a character(1). Path to a SQLite database.
#' @param table_name a character(1). Name of a table in the database. It will
#' be created if missing.
#' @param verbose a logical(1).
#'
#' @return gpkg_file (invisible).
#'
#' @export
#'
sf_data_to_db <- function(gpkg_file, sqlite_file, table_name,
                          verbose = FALSE) {

  # Reading geodata.
  data_tb <-
    gpkg_file |>
    sf::read_sf() |>
    sf::st_drop_geometry()

  # Opening a connection to the database.
  db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)

  # Ensuring the DB table exists.
  if (!DBI::dbExistsTable(db_con, table_name)) {
    message(sprintf("Creating the table %s ...", table_name))
  }

  # Writing data to the data base.
  tryCatch({
    if (verbose) {
      message("Loading data to the database...")
    }
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
    if (verbose) {
      message("Closing database's connection...")
    }
    DBI::dbDisconnect(db_con)
    gc()
  })

  invisible(gpkg_file)
}
