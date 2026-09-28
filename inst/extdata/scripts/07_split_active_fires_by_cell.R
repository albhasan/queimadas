library(DBI)
library(dplyr)
library(tidyr)

sqlite_file <-
    "/home/alber/Documents/data/r_packages/queimadas/fire_grid.sqlite"
table_name <- "fire_foci_grid"

out_dir <- "/home/alber/Documents/results/r_packages/queimadas"

stopifnot("Database file not found!" = file.exists(sqlite_file))
stopifnot("Output directory not found!" = dir.exists(out_dir))

#---- Get data from the database ----

db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)

brazil_ymc_tb <-
  db_con |>
  get_brazil_year_month_cell(table_name = table_name) |>
  dplyr::collect()

DBI::dbDisconnect(conn = db_con)
rm(db_con)



#---- Forecast using the queimadas approach ----

sat_char <- c(
  "AQUA_M-T",
  "NOAA-12",
  "NPP-375-PM",
  "NPP-375D"
)

stopifnot(
  "Reference satellite not found in data!" =
    all(sat_char %in% unique(brazil_ymc_tb[["satelite"]]))
)

sat_tb <-
  sat_char |>
  tidyr::expand_grid(sat_char) |>
  magrittr::set_colnames(c("satelite_x", "satelite_y")) |>
  dplyr::filter(satelite_x != satelite_y)

stopifnot("Invalid number of columns!" = ncol(sat_tb) == 2)

# Split data by cell and write them to disc.
cell_tb <-
  brazil_ymc_tb |>
  tidyr::nest(
    data = tidyselect::everything(),
    .by = path_row
  ) |>
  dplyr::mutate(
    out_file = purrr::map2(
      .x = data,
      .y = path_row,
      .f = function(data, path_row, out_dir) {
        out_file <- file.path(
          out_dir,
          "cell_ts",
          paste0("active-fires_", path_row, ".RDS")
        )
        saveRDS(
          object = data,
          file = out_file
        )
        return(out_file)
      },
      out_dir = out_dir
    )
  )
