library(dplyr)
library(geobr)
library(ggplot2)
library(purrr)
library(sf)
library(stringr)
library(tidyr)

library(queimadas)

# TODO: There are many gaps in the grid. To fill them in, try using the
#       12-month regression parameters when there is no way to use the 1-month
#       regression.

#---- Configuration ----

rds_dir <- "/home/alber/Documents/results/r_packages/queimadas/cell_lm"
stopifnot("Directory with RDS files not found!" = dir.exists(rds_dir))

grid_gpkg <-
  "/home/alber/Documents/data/r_packages/queimadas/grade_tm_util.gpkg"
stopifnot("Grid geopackages file not found!" = file.exists(grid_gpkg))

out_dir <- "/home/alber/Documents/github/slides/queimadas/slides"
stopifnot("Output directory not found!" = dir.exists(out_dir))

# grid_dir <- file.path(out_dir, "grid")
# stopifnot(
#   "Output directory for grid figures not found!" =
#   dir.exists(grid_dir)
# )

plot_size_a5_ls <- get_paper_size(name = "A5", orientation = "ls")

# Breaks used for ploting data.
pvalue_breaks_tb <- tibble::tibble(
  pbreaks = sort(c(-Inf, -1, -0.10, -0.05, -0.01, 0.01, 0.05, 0.10, 1, Inf)),
  pcolors = scales::viridis_pal(option = "magma")(length(pbreaks)),
  pnames = c(
    NA,
    "p >= -1.00",
    "p >= -0.10",
    "p >= -0.05",
    "p >= -0.01",
    "p >= 0.01",
    "p >= 0.05",
    "p >= 0.10",
    "p >= 1.00",
    NA
  )
)



#---- Script ----

# Ensure directories  exist.
out_dir_figures <- file.path(out_dir, "figures_R")
out_dir_tables <- file.path(out_dir, "tables_R")
if (dir.exists(out_dir_figures) == FALSE){
  dir.create(out_dir_figures)
}
if (dir.exists(out_dir_tables) == FALSE){
  dir.create(out_dir_tables)
}



# Read the spatial grid.
grid_sf <-
  grid_gpkg |>
  sf::read_sf() |>
  dplyr::select(path_row)

# Use additional data to crop the grid to fit Brazil.
biomes_sf <- geobr::read_biomes(year = 2025, simplified = TRUE)
biomes_sf <- sf::st_transform(biomes_sf, crs = sf::st_crs(grid_sf))

# Get the path & rows covering Brazil.
path_row_br <-
  grid_sf |>
  sf::st_intersection(y = biomes_sf) |>
  sf::st_drop_geometry() |>
  dplyr::pull(path_row) |>
  unique() |>
  sort()

# Filter the grid.
grid_sf <-
  grid_sf |>
  dplyr::filter(path_row %in% path_row_br)

# Read the data.
data_tb <-
  rds_dir |>
  list.files(
    pattern = "*.RDS$",
    full.names = TRUE
  ) |>
  tibble::as_tibble() |>
  dplyr::rename(filepath = "value") |>
  dplyr::mutate(
    data_tb = purrr::map(
      .x = filepath,
      .f = readRDS
    )
  ) |>
  tidyr::unnest(data_tb)

# Get the R squared data ready for plotting.

rsquared_lm12_tb <-
  data_tb |>
  dplyr::select(
    path_row, satelite_x, satelite_y,
    lm12 = rsquared_12,
    lm_12_model_param
  ) |>
  tidyr::unnest(lm12, names_sep = "_") |>
  dplyr::mutate(
   pvalue_tb = purrr::map(
      .x=  lm_12_model_param,
      .f = get_pvalues_from_lm12_params,
      prefix = "lm12_"
    )
  ) |>
  tidyr::unnest(pvalue_tb) |>
  tidyr::nest(
    .by = tidyselect::all_of(x = c("satelite_x", "satelite_y")),
    .key = "lm12_sp",
  ) |>
  dplyr::mutate(
    lm12_sp = purrr::map(
      .x = lm12_sp,
      .f = function(lm12_sp, grid_sf) {
        dplyr::left_join(
          x = grid_sf,
          y = lm12_sp,
          by = "path_row"
        )
      },
      grid_sf = grid_sf
    )
  )

rsquared_lm01_tb <-
  data_tb |>
  dplyr::select(
    path_row, satelite_x, satelite_y,
    lm01 = rsquared_01,
    lm_01_model_param
  ) |>
  # Get the pvalues.
  dplyr::mutate(
   pvalue_tb = purrr::map(
      .x=  lm_01_model_param,
      .f = get_pvalues_from_lm01_params,
      prefix = "lm01_"
    )
  ) |>
  dplyr::mutate(
    lm01 = purrr::map(
      .x = lm01,
      .f = tibble::as_tibble
    )
  ) |>
  # Join the data in lm01 and pvalue_tb.
  dplyr::mutate(
    lm01 = purrr::map2(
      .x = lm01,
      .y = pvalue_tb,
      .f = function(lm01, pvalue_tb){
        if (nrow(pvalue_tb) == 0) {
          return(lm01)
        }
        return(dplyr::left_join(
          x = lm01,
          y = pvalue_tb,
          by = dplyr::join_by(month == month)
        ))
      }
    )
  ) |>
  dplyr::select(-pvalue_tb) |>
  tidyr::unnest(lm01, names_sep = "_") |>
  dplyr::mutate(
    lm01_month = stringr::str_sub(
      string = lm01_month,
      start = 7L,
      end = -1L
    )
  ) |>
  tidyr::nest(
    .by = tidyselect::all_of(x = c("satelite_x", "satelite_y")),
    .key = "lm01_sp",
  ) |>
  dplyr::mutate(
    lm01_sp = purrr::map(
      .x = lm01_sp,
      .f = sp_join_month,
      grid_sf = grid_sf
    )
  )

rsquared_tb <-
  dplyr::left_join(
    x = rsquared_lm01_tb,
    y = rsquared_lm12_tb,
    by = dplyr::join_by(satelite_x == satelite_x, satelite_y == satelite_y)
  )

rm(rsquared_lm01_tb)
rm(rsquared_lm12_tb)

# Recode the p-values into classes.

brazil_sf <- 
  geobr::read_state(
    year = 2025,
    simplified = TRUE,
    output = "sf",
    showProgress = FALSE,
    cache = TRUE,
    verbose = FALSE
  )

rsquared_tb <-
  rsquared_tb |>
  # Plot rsquared.
  dplyr::mutate(
    plot_lm12_rsqr = purrr::map(
      .x = lm12_sp,
      .f = get_plot_sf,
      var = lm12_rsqr_adj,
      polygons_sf = brazil_sf
    )
  ) |>
  dplyr::mutate(
    plot_lm01_rsqr = purrr::map(
      .x = lm01_sp,
      .f = get_plot_lm01_rsqr,
      var = lm01_rsqr_adj,
      polygons_sf = brazil_sf
    )
  ) |>
  # Plot pvalues.
  dplyr::mutate(
    plot_lm12_pvalue_intercept = purrr::map(
      .x = lm12_sp,
      .f = get_plot_lm_pvalues,
      breaks_tb = pvalue_breaks_tb,
      var_pvalue = lm12_pvalue_intercept,
      polygons_sf = brazil_sf
    ),
    plot_lm12_pvalue_slope = purrr::map(
      .x = lm12_sp,
      .f = get_plot_lm_pvalues,
      breaks_tb = pvalue_breaks_tb,
      var_pvalue = lm12_pvalue_slope,
      polygons_sf = brazil_sf
    ),
  ) |>
  dplyr::mutate(
    plot_lm01_pvalue = purrr::map(
      .x = lm01_sp,
      .f = get_plot_lm01_pvalues,
      breaks_tb = pvalue_breaks_tb,
      var_pvalue_intercept = lm01_lm01_pvalue_intercept,
      var_pvalue_slope = lm01_lm01_pvalue_slope,
      polygons_sf = brazil_sf
    )    
  ) |>
  # Write lm12 rsqr maps to disc!
  dplyr::mutate(
    path_plot_lm12_rsqr = purrr::pmap_chr(
      .l = list(plot_lm12_rsqr, satelite_x, satelite_y),
      .f = save_plot_lm12_rsqr,
      plot_size = plot_size_a5_ls,
      out_dir = out_dir_figures
    )
  ) |>
  # Write lm12 p.value maps to disc!
  dplyr::mutate(
    path_plot_lm12_pvalue_intercept = purrr::pmap_chr(
      .l = list(plot_lm12_pvalue_intercept, satelite_x, satelite_y),
      .f = save_plot_lm12_pvalue_intercept,
      plot_size = plot_size_a5_ls,
      out_dir = out_dir_figures
    ),
    path_plot_lm12_pvalue_slope = purrr::pmap_chr(
      .l = list(plot_lm12_pvalue_slope, satelite_x, satelite_y),
      .f = save_plot_lm12_pvalue_slope,
      plot_size = plot_size_a5_ls,
      out_dir = out_dir_figures
    )
  ) |>
  # Write lm01 rsqr maps to disc!
  # NOTE: The file paths are inside plot_lm01_rsqr_tb.
  dplyr::rename(plot_lm01_rsqr_tb = plot_lm01_rsqr) |>
  dplyr::mutate(
    plot_lm01_rsqr_tb = purrr::pmap(
      .l = list(plot_lm01_rsqr_tb, satelite_x, satelite_y),
      .f = save_plot_lm01_rsqr,
      plot_size = plot_size_a5_ls,
      out_dir = out_dir_figures
    )
  ) |>
  # Write lm01 pvalues (intercept and slope) to disc!
  # NOTE: The file paths are inside plot_lm01_rsqr_tb.
  dplyr::rename(plot_lm01_pvalue_tb = plot_lm01_pvalue) |>
  dplyr::mutate(
    plot_lm01_pvalue_intercept = purrr::pmap(
      .l = list(plot_lm01_pvalue_tb, satelite_x, satelite_y),
      .f = save_plot_lm01_pvalue_intercept,
      plot_size = plot_size_a5_ls,
      out_dir = out_dir_figures
    ),
    plot_lm01_pvalue_slope = purrr::pmap(
      .l = list(plot_lm01_pvalue_tb, satelite_x, satelite_y),
      .f = save_plot_lm01_pvalue_slope,
      plot_size = plot_size_a5_ls,
      out_dir = out_dir_figures
    )
  )

# Get r-squared and p.value tables ready for exporting to CSV.

export_lm12_rsquared_tb <-
  rsquared_tb |>
  dplyr::select(satelite_x, satelite_y, lm12_sp) |>
  dplyr::mutate(
    lm12_tb = purrr::map(
      .x = lm12_sp,
      .f = sf::st_drop_geometry
    ),
    lm12_tb = purrr::map(
      .x = lm12_tb,
      .f = function(lm12_tb) {
        res <-
          lm12_tb |>
          dplyr::select(path_row, lm12_rsqr, lm12_rsqr_adj,
            lm12_pvalue_intercept, lm12_pvalue_slope)
        return(res)
      }
    )
  ) |>
  dplyr::select(-lm12_sp) |>
  unnest(lm12_tb) |>
  dplyr::select(satelite_x, satelite_y, path_row, lm12_rsqr, lm12_rsqr_adj,
      lm12_pvalue_intercept, lm12_pvalue_slope) |>
  readr::write_csv(
    file = file.path(out_dir_tables, "lm12_rsquared_tb.csv"),
    progress = FALSE
  )

export_lm01_rsquared_tb <-
  rsquared_tb |>
  dplyr::select(satelite_x, satelite_y, lm01_sp) |>
  dplyr::mutate(
    lm01_tb = purrr::map(
      .x = lm01_sp,
      .f = sf::st_drop_geometry
    )
  ) |>
  dplyr::select(-lm01_sp) |>
  tidyr::unnest(lm01_tb) |>
  tidyr::unnest(plot_data) |>
  dplyr::select(satelite_x, satelite_y, path_row, lm01_month, lm01_rsqr,
      lm01_rsqr_adj,
      lm01_pvalue_intercept = lm01_lm01_pvalue_intercept,
      lm01_pvalue_slope = lm01_lm01_pvalue_slope) |>
  readr::write_csv(
    file = file.path(out_dir_tables, "lm01_rsquared_tb.csv"),
    progress = FALSE
  )
