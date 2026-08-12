library(DBI)
library(dplyr)
library(logger)
library(purrr)
library(readr)

library(queimadas)

logger::log_threshold(INFO)

logger::log_info("Script 01_download_data starting...")

logger::log_info("Reading parameters...")
tmp_dir <- tempdir()
zip_dir <- file.path(tmp_dir, "zip_dir")
csv_dir <- file.path(tmp_dir, "csv_dir")
sqlite_file <- "/home/alber/Documents/data/r_packages/queimadas/fire.sqlite"
table_name <- "fire_foci"
# NOTE: Storing data_pas as character instead of date eases posterior analysis.
c_types <- "ddccccccdnncd"

stopifnot("Database directory not found!" = dir.exists(dirname(sqlite_file)))
stopifnot("Database already exists!" = !file.exists(sqlite_file))
stopifnot("Temporal directory not found!" = dir.exists(tmp_dir))

if (dir.exists(zip_dir) == FALSE) {
  dir.create(path = zip_dir)
}
if (dir.exists(csv_dir) == FALSE) {
  dir.create(path = csv_dir)
}

logger::log_info("Check if zip files were already downloaded...")
if (length(list.files(zip_dir)) == 0) {
  # Download zip files from INPE Queimadas.
  logger::log_info("Downloading zip files from Queimadas...")
  zip_files <-
    download_data(
      out_dir = zip_dir,
      data_url = get_data_url(),
      wait_time = 5,
      overwrite_files = FALSE,
      quiet = FALSE
    )
} else {
  logger::log_warn(
    "Directory not empty. Assuming the zip files were already downloaded!"
  )
  zip_files <-
    list.files(
      path = zip_dir,
      pattern = "*.zip",
      recursive = TRUE,
      full.names = TRUE
    )
}

logger::log_info("Checking if data files were already unzipped...")
if (length(list.files(csv_dir)) == 0) {
  logger::log_info("Unzipping files from Queimadas...")
  csv_files <-
    unzip_files(
      files = zip_files,
      out_dir = csv_dir
    )
} else {
  logger::log_warn(
    "Directory not empty. Assuming the files were already unzipped!"
  )
  csv_files <-
    list.files(
      path = csv_dir,
      pattern = "*csv",
      recursive = TRUE,
      full.names = TRUE
    )
}

if (length(zip_files) != length(csv_files)) {
  logger::log_error("Amount of ZIP and CSV files must match!")
  stop("Amount of ZIP and CSV files must match!")
}

# NOTE: To connect is to create the database file!
db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)
load_database(
  db_con = db_con,
  file_paths = csv_files,
  c_types = c_types,
  table_name = table_name,
  disconnect = FALSE
)

logger::log_info("Script 01_download_data finished!")
