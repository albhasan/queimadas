#!/usr/bin/env Rscript
###############################################################################
# PROCESS THE ACTIVE FIRES IN EACH CELL
#------------------------------------------------------------------------------
# ./fire_by_cell.R
# -f /home/alber/Documents/results/r_packages/queimadas/cell_ts/active-fires_233_067.RDS
# -o /home/alber/Documents/results/r_packages/queimadas/cell_ts_lm01
###############################################################################

suppressMessages(require(dplyr))
suppressMessages(require(optparse))

require(queimadas)



#---- Take parameters from command line ----

option_list <-
  list(
    optparse::make_option(
      opt_str = c("-f", "--file"),
      type = "character",
      help = "A file with active fire data."
    ),
    optparse::make_option(
      opt_str = c("-o", "--out"),
      type = "character",
      help = "A directory for storing the results."
    )
  )

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

rds_file <- opt$file
out_dir <- opt$out

stopifnot("RDS file not found!" = file.exists(rds_file))
stopifnot("Output directory not found!" = dir.exists(out_dir))

data_tb <- readRDS(file = rds_file)

if ("path_row" %in% colnames(data_tb)) {
  message(sprintf(
    "Found %s unique cells (path row) in data.",
    length(unique(data_tb[["path_row"]]))
  ))
}



#---- Additional parameters ----

confidence_level <- 0.95

sat_char <- get_satellites()

stopifnot(
  "Reference satellite not found in data!" =
    all(sat_char %in% unique(data_tb[["satelite"]]))
)

plot_size_a5_ls <- get_paper_size(name = "A5", orientation = "ls")



#--- Get combination of satellites ---

sat_tb <-
  sat_char |>
  tidyr::expand_grid(sat_char) |>
  magrittr::set_colnames(c("satelite_x", "satelite_y")) |>
  dplyr::filter(satelite_x != satelite_y)

stopifnot("Invalid number of columns!" = ncol(sat_tb) == 2)

sat_tb <-
  sat_tb |>
  # Get x & y data.
  dplyr::mutate(
    x_df = purrr::map(
      .x = satelite_x,
      .f = get_sat_data,
      data_df = data_tb
    ),
    y_df = purrr::map(
      .x = satelite_y,
      .f = get_sat_data,
      data_df = data_tb
    )
  ) |>
  # Remove non-overlaping satellites.
  dplyr::mutate(
    overlap_ts = purrr::map2_int(
      .x = x_df,
      .y = y_df,
      .f = overlap_len,
      cname = "period"
    )
  ) |>
  dplyr::filter(overlap_ts > 0) |>
  dplyr::select(-overlap_ts)

sat_01_tb <-
  sat_tb |>
  # Fit a linear model using overlapping x & y data.
  dplyr::mutate(
    # Fit a model for each month.
    lm_data_month = purrr::map2(
      .x = x_df,
      .y = y_df,
      .f = fit_lm_01_months,
      formula = "y ~ x",
      clevel = confidence_level
    )
  ) |>
  dplyr::mutate(
    # Get the fitted models in their own column.
    lm_01_models = purrr::map(
      .x = lm_data_month,
      .f = function(data_ls) {
        data_tb_ls <- lapply(X = data_ls, FUN = function(a) {
          return(a[["model"]])
        })
      }
    ),
    # Get the fitted model data in a column.
    lm_01_data = purrr::map(
      .x = lm_data_month,
      .f = function(data_ls, data_names) {
        data_tb_ls <- lapply(X = data_ls, FUN = function(a) {
          data_tb <- tibble::as_tibble(a[["data"]])
        })
        return(dplyr::bind_rows(lnames2df(df_ls = data_tb_ls, cname = "month")))
      }
    )
  ) |>
  dplyr::select(-lm_data_month) |>
  # Compute models statistics r squred and adjusted r squared.
  dplyr::mutate(
    rsquared = purrr::map(
      .x = lm_01_models,
      .f = function(lm_models){
        rsqr_tb <- data.frame(
          rsqr = vapply(
            X = lm_models,
            FUN = get_lm_r2,
            adjusted = FALSE,
            numeric(1)
          ),
          rsqr_adj = vapply(
            X = lm_models,
            FUN = get_lm_r2,
            adjusted = TRUE,
            numeric(1)
          )
        )
        rsqr_tb <- tibble::rownames_to_column(rsqr_tb, var = "month")
        return(rsqr_tb)
      }
    )
  ) |>
  # Get the models parameters as a tibble.
  dplyr::mutate(
    lm_01_model_param = purrr::map(
      .x = lm_01_models,
      .f = function(models_ls) {
        purrr::map(
          .x = models_ls,
          .f = broom::tidy,
          # TODO: check if broom computes the CI the same way as stats::predict
          conf.int = TRUE,
          conf.level = confidence_level
        ) |>
          purrr::list_rbind(names_to = "month")
      }
    )
  ) |>
  # Create plots for the 12 models of each row.
  dplyr::mutate(
    plot_lm_01 = purrr::pmap(
      .l = list(
        x = satelite_x,
        y = satelite_y,
        data_df = lm_01_data
      ),
      .f = get_plot_ref_sats_01
    )
  ) |>
  # Keep satellite combinations that have enough models (12).
  dplyr::mutate(
    n_models = purrr::map_int(
      .x = lm_01_models,
      .f = length
    )
  ) |>
  dplyr::filter(n_models == 12) |>
  # Predict future and past using the fitted models.
  dplyr::mutate(
    forecast_01_df = purrr::map2(
      .x = lm_01_models,
      .y = x_df,
      .f = function(lm_ls, x_df) {
        pred_ls <- predict_ci_01(
          lm_ls = lm_ls,
          new_data = x_df,
          clevel = confidence_level
        )
        pred_df <- do.call(rbind, pred_ls)
        rownames(pred_df) <- NULL
        return(pred_df)
      }
    )
  ) |>
  # Plot forecast month by month.
  dplyr::mutate(
    plot_queimadas_01 = purrr::pmap(
      .l = list(
        x_df = x_df,
        y_df = y_df,
        forecast_df = forecast_01_df
      ),
      .f = get_plot_queimadas_forecast
    )
  )


# Get the path and row from the input file's name.
file_path_row <-
  rds_file |>
  basename() |>
  tools::file_path_sans_ext() |>
  stringr::str_match(
    pattern = "[0-9]{3}_[0-9]{3}"
  ) |>
  as.character()

# Write forecast plots to disc.
for (i in seq_len(nrow(sat_01_tb))) {
  sat_x <- sat_01_tb[["satelite_x"]][[i]]
  sat_y <- sat_01_tb[["satelite_y"]][[i]]
  #p <- sat_01_tb[["plot_lm_01"]][[i]]
  p <- sat_01_tb[["plot_queimadas_01"]][[i]]
  plot_file <- file.path(
    out_dir,
    paste0(
      "plot_queimadas_lm_01_",
      sat_y, "_along_", sat_x, "_", file_path_row,
      ".png"
    )
  )
  ggplot2::ggsave(
    filename = plot_file,
    plot = p,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}

# Save a table with regression parameters.
out_file <-
  rds_file |>
  basename() |>
  tools::file_path_sans_ext()
out_file <-
  file.path(
    out_dir,
    paste0(out_file, "_lm-params.RDS")
  )

sat_01_tb |>
  dplyr::mutate(path_row = file_path_row) |>
  dplyr::select(path_row, satelite_x, satelite_y, lm_01_model_param, rsquared) |>
  saveRDS(file = out_file)
