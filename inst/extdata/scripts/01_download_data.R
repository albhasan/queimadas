library(devtools)
devtools::load_all()

library(DBI)
library(dplyr)
library(purrr)
library(readr)

out_dir <- "/home/alber/Documents/data/queimadas/Brasil_todos_sats"
tmp_dir <- "/home/alber/Downloads/tmp/queimadasdata"
sqlite_file <- "~/Downloads/fire.sqlite"
table_name <- "fire_foci"

# NOTE: Storing data_pas as character instead of date eases posterior analysis.
#c_types <- "ddTcccccdnncd"
c_types <- "ddccccccdnncd"

# Check if files were already downloaded.
if (length(list.files(out_dir)) == 0) {
  # Download zip files from INPE Queimadas.
  message("Downloading files from Queimadas...")
  zip_files <-
    download_data(
      out_dir = "",
      data_url = get_data_url(),
      wait_time = 5,
      overwrite_files = FALSE,
      quiet = FALSE
    )
} else {
  warning("Directory not empty. Assuming the zip files were already downloaded!")
  zip_files <-
    list.files(
               path = out_dir,
               pattern = "*.zip",
               recursive = TRUE,
               full.names = TRUE
    )
}

# Check if files were already unzipped.
if (length(list.files(tmp_dir)) == 0) {
  message("Unzipping files from Queimadas...")
  csv_files <- 
    unzip_files(
      files = zip_files,
      out_dir = tmp_dir
    )
} else {
  warning("Directory not empty. Assuming the files were already unzipped!")
  csv_files <-
    list.files(
               path = tmp_dir,
               pattern = "*csv",
               recursive = TRUE,
               full.names = TRUE
    )
}

stopifnot("Amount of ZIP and CSV files must match!" =
          length(zip_files) == length(csv_files))

if (file.exists(sqlite_file)) {
  # NOTE: To connect is to create the database file!
  db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)
} else {
  db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)
  load_database(
    db_con = db_con,
    file_paths = csv_files,
    c_types = c_types,
    table_name = table_name,
    disconnect = FALSE
  )
}
