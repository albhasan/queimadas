library(dplyr)
library(geobr)
library(ggplot2)
library(purrr)
library(sf)
library(tidyr)

library(queimadas)


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

#' Helper function Get a base map
#' 
#' @description 
#' Helper function for getting a base map givne the plot data.
#'
#' @param plot_data an sf object.
#' @param var name of a variable in plot_data.
#'
#' @return a ggplot2 object.
#'
get_base_map <- function(plot_data, var) {
  p <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = drop_na(plot_data),
      mapping = ggplot2::aes(fill = {{ var }})
    )
  return(p)
}

#' Helper function. Get a base plot of the frequency of categories of p-values
#'
#' @description 
#' Helper function for creating a histogram of the frequency of the categories
#' of p-values in the grid.
#'
#' @param plot_data a tibble. Data for the plot.
#' colors.
#'
#' @return a ggplto2 object.
#'
get_base_hist <- function(plot_data) {
  p <-
    plot_data |>
    sf::st_drop_geometry() |>
    tidyr::drop_na(rsqr_adj) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        y = pinterval,
        fill = pcolor
      )
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      x = "Frequency (number of cells)",
      y = "p-value"
    )
  return(p)
}

#' Get the inteval's name
#'
#' @description
#' Given a value and an sorted numeric of breaks, return the name corresponding
#' to x's interval.
#'
#' @param x a numeric.
#' @param breaks a numeric (see `base::findInterval`)
#' @param bnames a character. The names for each interval defined by the given
#' breaks.
#'
find_interval_name <- function(x, breaks, bnames) {
  return(bnames[findInterval(x = x, vec = breaks)])
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

# Get the R squared data ready for plotting.
rsquared_tb <-
  data_tb |>
  dplyr::select(path_row, satelite_x, satelite_y, rsquared) |>
  tidyr::unnest(rsquared) |>
  dplyr::group_by(satelite_x, satelite_y) |>
  tidyr::nest(.key = "dat") |>
  # Join the data to the grid. 
  dplyr::mutate(
    sp_join = purrr::map(
      .x = dat,
      .f = sp_join_month,
      grid_sf = grid_sf
    )
  ) |>
  dplyr::select(satelite_x, satelite_y, sp_join) |>
  tidyr::unnest(sp_join) |>
  # Recode the p-values into classes.
  dplyr::mutate(
    plot_data = purrr::map(
      .x = plot_data,
      .f = function(plot_data, breaks_tb) {
        plot_data <- 
          plot_data |> 
          dplyr::mutate(
            pinterval = purrr::map_chr(
              .x = rsqr_adj,
              .f = find_interval_name,
              breaks = breaks_tb[["pbreaks"]],
              bnames = breaks_tb[["pnames"]]
            ),
            pinterval = factor(
              x = pinterval,
              levels = breaks_tb[["pnames"]],
              ordered = TRUE
            ),
            pcolor = breaks_tb[["pcolors"]][pinterval]
          ) 
        return(plot_data)
      },
      breaks_tb = pvalue_breaks_tb
    )
  )

# Get the grid plots.
rsquared_tb <-
  rsquared_tb |>
  # Get a basic map of the data on the grid. 
  dplyr::mutate(
    plot_rsqr_adj_grid = purrr::map(
      .x = plot_data,
      .f = get_base_map,
      #var = rsqr_adj,
      var = pinterval
    )
  ) |>
  # Create file names for storing grid plots.
  dplyr::mutate(
    title_plot_grid = sprintf("x: %s, y: %s, %s", satelite_x, satelite_y, month),
    out_file_grid = file.path(
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
  # Add a title to each grid plot.
  dplyr::mutate(
      plot_rsqr_adj_grid = purrr::map2(
        .x = plot_rsqr_adj_grid,
        .y = title_plot_grid,
        .f = function(plot_obj, title) {
          plot_obj +
            ggplot2::labs(title = title)
        }
      )  
  ) |>
  # Write grid plots to disc.
  dplyr::mutate(
    plot_rsqr_adj_grid_written_to = purrr::map2_chr(
      .x = plot_rsqr_adj_grid,
      .y = out_file_grid,
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

# Get the frequency plots.
rsquared_tb <-
  rsquared_tb |>
  # Add the Adjusted R squared frequency plot.
  dplyr::mutate(
    plot_rsqr_adj_hist = purrr::map(
      .x = plot_data,
      .f = get_base_hist
    )
  ) |>
  # Create file names for storing each frequency plot.
  dplyr::mutate(
    title_plot_hist = sprintf("x: %s, y: %s, %s", satelite_x, satelite_y, month),
    out_file_hist = file.path(
      grid_dir,
      paste0(
        "plot_hist_x_",
        satelite_x,
        "_y_",
        satelite_y,
        "_",
        month,
        "_rsqr-adj.png"
      )
    )
  ) |>
  # Add a title to each frequency plot.
  dplyr::mutate(
      plot_rsqr_adj_hist = purrr::map2(
        .x = plot_rsqr_adj_hist,
        .y = title_plot_hist,
        .f = function(plot_obj, title) {
          plot_obj +
            ggplot2::labs(title = title)
        }
      )  
  ) |>
  # Write to frequancy plots to disc.
  dplyr::mutate(
    plot_rsqr_adj_hist_written_to = purrr::map2_chr(
      .x = plot_rsqr_adj_hist,
      .y = out_file_hist,
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
      .f = get_base_map,
      var = estimate
    ),
    plot_pvalue = purrr::map(
      .x = plot_data,
      .f = get_base_map,
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
