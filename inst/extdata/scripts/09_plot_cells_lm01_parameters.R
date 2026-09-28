library(dplyr)
library(geobr)
library(ggplot2)
library(purrr)
library(sf)
library(tidyr)



#---- Configuration ----

rds_dir <- "/home/alber/Documents/results/r_packages/queimadas/cell_ts_lm01"
stopifnot("Directory with RDS files not found!" = dir.exists(rds_dir))

grid_gpkg <-
  "/home/alber/Documents/data/r_packages/queimadas/grade_tm_util.gpkg"
stopifnot("Grid geopackages file not found!" = file.exists(grid_gpkg))

out_dir <- "/home/alber/Documents/github/slides/queimadas/slides/figures"
grid_dir <- file.path(out_dir, "grid")
stopifnot("Output directory not found!" = dir.exists(out_dir))
stopifnot(
  "Output directory for grid figures not found!" = 
  dir.exists(grid_dir)
)

plot_size_a5_ls <- get_paper_size(name = "A5", orientation = "ls")

#---- Utility functions ----

#' Get data for the monthly plots
#'
#' @description
#' Helper function. Build monthly sf objects given a grid and a data frame of
#' yearly data.
#'
#' @param dat a tibble.
#' @param grid_sf an sf object representing a grid.
#'
#' @return a tibble.
#'
sp_join_month <- function(dat, grid_sf) {
  stopifnot("Month column not found!" = "month" %in% colnames(dat))
  x <-
    dat |>
      dplyr::group_by(month) |>
      tidyr::nest(.key = "dat_month") |>
      dplyr::mutate(
        plot_data = purrr::map(
          .x = dat_month,
          .f = function(dat_month, grid_sf) {
            res <-
              grid_sf |>
                dplyr::left_join(
                  y = dat_month,
                  by = "path_row",
                  keep = FALSE
                )
                #dplyr::select(rsqr_adj)
            return(res)
          },
          grid_sf = grid_sf
        )
      ) |>
      dplyr::select(month, plot_data)
  return(x)
}



#' Helper funciton. Get a base map
#' 
#' @description 
#' Helper funciton for getting a base map givne the plot data.
#'
#' @param plot_data an sf object.
#' @param var name of a variable in plot_data.
#'
#' @return a ggplot2 object.
#'
get_base_plot <- function(plot_data, var) {
  p <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = plot_data,
      mapping = ggplot2::aes(fill = {{ var }})
    )
  return(p)
}




#---- Script ----

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

# Get the R squared data ready or plot.
rsquared_tb <-
  data_tb |>
  dplyr::select(path_row, satelite_x, satelite_y, rsquared) |>
  tidyr::unnest(rsquared) |>
  dplyr::group_by(satelite_x, satelite_y) |>
  tidyr::nest(.key = "dat") |>
  dplyr::mutate(
    sp_join = purrr::map(
      .x = dat,
      .f = sp_join_month,
      grid_sf = grid_sf
    )
  ) |>
  dplyr::select(satelite_x, satelite_y, sp_join) |>
  tidyr::unnest(sp_join) |>
  # Get a basic plot of the data.
  dplyr::mutate(
    plot_rsqr_adj = purrr::map(
      .x = plot_data,
      .f = get_base_plot,
      var = rsqr_adj
    )
  ) |>
  # Create a file name for storing each plot.
  dplyr::mutate(
    title = sprintf("x: %s, y: %s, %s", satelite_x, satelite_y, month),
    out_file = file.path(
      grid_dir,
      paste0(
        "plot_grid_x_",
        satelite_x,
        "_y_",
        satelite_y,
        "_",
        month,
        "_rsqr-adj.png"
      )
    )
  ) |>
  # Add a title to each plot.
  dplyr::mutate(
      plot_rsqr_adj = purrr::map2(
        .x = plot_rsqr_adj,
        .y = title,
        .f = function(plot_obj, title) {
          plot_obj +
            ggplot2::labs(title = title)
        }
      )  
  ) |>
  # Write to disc.
  dplyr::mutate(
    written_to = purrr::map2_chr(
      .x = plot_rsqr_adj,
      .y = out_file,
      .f = function(plot, file, size) {
        ggplot2::ggsave(
          filename = file,
          plot = plot,
          width = size[["width"]],
          height = size[["height"]],
          units = size[["units"]]
        )
        return(file)
      },
      size = plot_size_a5_ls
    )
  )

lm_param_tb <-
  data_tb |>
  dplyr::select(path_row, satelite_x, satelite_y, lm_01_model_param) |>
  tidyr::unnest(lm_01_model_param) |>
  dplyr::group_by(satelite_x, satelite_y, term) |>
  tidyr::nest(.key = "dat") |>
  # Join data to spatial grid.
  dplyr::mutate(
    sp_join = purrr::map(
      .x = dat,
      .f = sp_join_month,
      grid_sf = grid_sf
    )
  ) |>
  tidyr::unnest(sp_join) |>
  #NOTE: plot_data contains more variables worth of plotting!
  dplyr::select(satelite_x, satelite_y, month, term, plot_data) |>
  dplyr::mutate(
    term = dplyr::if_else(
      condition = term %in% c("(Intercept)") ,
      true = "intercept",
      false = "slope"
    )
  ) |>
  # Create a basic plot.
  dplyr::mutate(
    plot_estimate = purrr::map(
      .x = plot_data,
      .f = get_base_plot,
      var = estimate
    ),
    plot_pvalue = purrr::map(
      .x = plot_data,
      .f = get_base_plot,
      var = p.value
    )
  ) |>
  # Create a title and a filepath for storing the plot.
  dplyr::mutate(
    title_estimate = sprintf("x: %s, y: %s, %s, %s", satelite_x, satelite_y,
                             month, term),
    title_pvalue = sprintf("x: %s, y: %s, %s, %s (p.value)", satelite_x,
                           satelite_y, month, term),
    out_file_estimate = file.path(
      grid_dir,
      paste0(
        "plot_grid_x_",
        satelite_x,
        "_y_",
        satelite_y,
        "_",
        month,
        "_",
        term,
        ".png"
      )
    ),
    out_file_pvalue = paste0(tools::file_path_sans_ext(out_file_estimate),
                             "_pvalue.png")
  )  |>
  # Add a title to each plot.
  dplyr::mutate(
      plot_estimate = purrr::map2(
        .x = plot_estimate ,
        .y = title_estimate,
        .f = function(plot_obj, title) {
          plot_obj +
            ggplot2::labs(title = title)
        }
      )  ,
      plot_pvalue = purrr::map2(
        .x = plot_pvalue,
        .y = title_pvalue,
        .f = function(plot_obj, title) {
          plot_obj +
            ggplot2::labs(title = title)
        }
      )  
  ) |>
  # Write to disc.
  dplyr::mutate(
    estimate_written_to = purrr::map2_chr(
      .x = plot_estimate,
      .y = out_file_estimate,
      .f = function(plot, file, size) {
        ggplot2::ggsave(
          filename = file,
          plot = plot,
          width = size[["width"]],
          height = size[["height"]],
          units = size[["units"]]
        )
        return(file)
      },
      size = plot_size_a5_ls
    ),
    pvalue_written_to = purrr::map2_chr(
      .x = plot_pvalue,
      .y = out_file_pvalue,
      .f = function(plot, file, size) {
        ggplot2::ggsave(
          filename = file,
          plot = plot,
          width = size[["width"]],
          height = size[["height"]],
          units = size[["units"]]
        )
        return(file)
      },
      size = plot_size_a5_ls
    )
  )
