#!/usr/bin/env Rscript
###############################################################################
# JOIN SPATAILY A CSV FILE TO A SPATIAL ENTITY
#------------------------------------------------------------------------------
# ./fire_sp_join.R \
# -c /home/alber/Documents/data/r_packages/queimadas/csv/focos_br_todos-sats_2019.csv \
# -g /home/alber/Documents/data/r_packages/queimadas/grade_tm_util.gpkg \
# -o /home/alber/Documents/data/r_packages/queimadas/results/gpkg
###############################################################################

suppressMessages(require(dplyr))
suppressMessages(require(optparse))
suppressMessages(require(readr))
suppressMessages(require(tools))
suppressMessages(require(sf))

sf::sf_use_s2(FALSE)

option_list <-
  list(
    optparse::make_option(
      opt_str = c("-c", "--csv"),
      type = "character",
      help = "A CSV file with active fire data from the Queimadas program."
    ),
    optparse::make_option(
      opt_str = c("-g", "--gpkg"),
      help = "A GeoPackage file with data (polygon) to join to the points in the CSV.",
      type = "character"
    ),
    optparse::make_option(
      opt_str = c("-o", "--out"),
      help = "A path to an output directory.",
      type = "character"
    )
  )

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

csv_file <- opt$csv
gpkg_file <- opt$gpkg
out_dir <- opt$out

stopifnot("CSV file not found!" = file.exists(csv_file))
stopifnot("GeoPackage file not found!" = file.exists(gpkg_file))
stopifnot(
  "Directory for outputs not found!" =
    dir.exists(dirname(out_dir))
)

# Read the shapefile.
polygons_sf <-
  gpkg_file |>
  sf::read_sf()

if (any(sf::st_is_valid(polygons_sf) == FALSE)) {
  polygonls_sf <- sf::st_make_valid(polygons_sf)
}
if (any(sf::st_is_valid(polygons_sf) == FALSE)) {
  polygons_sf <- sf::st_make_valid(polygons_sf)
}
if (any(sf::st_is_valid(polygons_sf) == FALSE)) {
  polygons_sf <- sf::st_make_valid(polygons_sf)
}
if (any(sf::st_is_valid(polygons_sf) == FALSE)) {
  stop(sprintf("Invalid polygon geometries: %s", gpkg_file))
}

# Read the CSV file.
data_tb <-
  csv_file |>
  readr::read_csv(
    col_types = "ddccccccdnncd"
  )

# Create the path to the output file.
out_file <-
  file.path(
    out_dir,
    paste0(
      tools::file_path_sans_ext(basename(csv_file)),
      "_joined_to_",
      tools::file_path_sans_ext(basename(gpkg_file)),
      ".gpkg"
    )
  )

stopifnot("Output file already exists!" = !file.exists(out_file))

# Validate colnames.
if (
  !all(c(
    "latitude", "longitude", "data_pas", "satelite", "pais", "estado",
    "municipio", "bioma", "numero_dias_sem_chuva", "precipitacao",
    "risco_fogo", "id_area_industrial", "frp"
  ) %in% colnames(data_tb))
) {
  # Try to convert columns to match.
  data_tb <-
    data_tb |>
    dplyr::rename(
      latitude = "Latitude",
      longitude = "Longitude",
      data_pas = "DataHora",
      satelite = "Satelite",
      pais = "Pais",
      estado = "Estado",
      municipio = "Municipio",
      bioma = "Bioma",
      numero_dias_sem_chuva = "DiaSemChuva",
      precipitacao = "Precipitacao",
      risco_fogo = "RiscoFogo",
      frp = "FRP"
    ) |>
    dplyr::mutate(
      id_area_industrial = NA
    ) |>
    dplyr::select(
      latitude, longitude, data_pas, satelite, pais, estado,
      municipio, bioma, numero_dias_sem_chuva, precipitacao,
      risco_fogo, id_area_industrial, frp
    )
}

points_sf <-
  data_tb |>
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    remove = FALSE,
    dim = "XY",
    crs = sf::st_crs(polygons_sf)
  )

# Join.
join_sf <-
  sf::st_join(
    x = points_sf,
    y = polygons_sf,
    join = st_intersects,
    suffix = c("poi_", "pol_"),
    left = TRUE,
    largest = FALSE
  )

sf::st_write(
  obj = join_sf,
  dsn = out_file,
  quiet = TRUE
)
