#' Load files to a database
#'
#' @description
#' Load the given files to a database of fire foci.
#'
#' @param db_con a connection to a data base.
#' @param file_paths a character. Path to CSV files.
#' @param c_types a character(1). Data types of the columns in input files.
#' @param table_name a character(1). Name of the table to creater in the
#' database.
#' @param disconnect a logical(1). Should the database connection be closed?
#'
#' @return the connection to the database (invisible).
#'
#' @export
#'
load_database <- function(db_con,
                          file_paths,
                          c_types = "ddTcccccdnncd",
                          table_name = "fire_foci",
                          disconnect = TRUE) {

  stopifnot(
    "All files must have the same extension!" =
      length(unique(tools::file_ext(file_paths))) == 1
  )
  stopifnot(
    "All input files must have the csv extension!" =
      all(tolower(tools::file_ext(file_paths)) == "csv")
  )
  stopifnot(any(
    "A data base connection object was expected!" =
      stringr::str_detect(
        string = class(db_con),
        pattern = c("Connection", "connection")
      )
  ))

  if (DBI::dbExistsTable(db_con, table_name)) {
    stop(sprintf("The table %s already exists!", table_name))
  } else {
    message(sprintf("Creating the table %s ...", table_name))
  }

  tryCatch({
    for (f_path in file_paths) {
      message(sprintf("Loading file %s to the database...", f_path))
      data_df <- readr::read_csv(f_path, col_types = c_types)
      DBI::dbWriteTable(
        conn = db_con,
        name = table_name,
        value = data_df,
        overwrite = FALSE,
        append = TRUE
      )
      rm(data_df)
    }
  },
  error = function(e) {
    message(paste("Error while loading data to the database", f_path, e))
  },
  finally = {
    if (disconnect) {
      message("Closing database's connection...")
      DBI::dbDisconnect(db_con)
    }
    gc()
  })

  invisible(db_con)
}
