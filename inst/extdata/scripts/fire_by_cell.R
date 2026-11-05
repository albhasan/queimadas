#!/usr/bin/env Rscript
###############################################################################
# RUN REGRESSIONS ON THE ACTIVE FIRES IN EACH CELL IN THE GRID
#------------------------------------------------------------------------------
# ./fire_by_cell.R
# -f /home/alber/Documents/results/r_packages/queimadas/cell_ts/active-fires_233_067.RDS
# -o /home/alber/Documents/results/r_packages/queimadas/cell_lm
###############################################################################

suppressMessages(require(dplyr))
suppressMessages(require(optparse))

require(queimadas)



#---- Take parameters from command line ----

if (interactive()) {
  rds_file <- "/home/alber/Documents/results/r_packages/queimadas/cell_ts/active-fires_233_067.RDS"
  out_dir <- "/home/alber/Documents/results/r_packages/queimadas/cell_lm"
} else {
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
  out_dir <- opt$out
  rds_file <- opt$file

}

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



#---- Utilitary functions ----

#' Get the path and row in the file name
#'
#' @description
#' Given a file path, return the path and row embedded in it.
#'
#' @param file_path a character(1). A path to a file (e.g.
#' "active-fires_233_067.RDS").
#'
#' @return a character(1). The path and row in the file name (e.g. "233_067").
#'
get_file_path_row <- function(file_path) {
  pr <-
    file_path |>
    basename() |>
    tools::file_path_sans_ext() |>
    stringr::str_match(
      pattern = "[0-9]{3}_[0-9]{3}"
    ) |>
    as.character()
  return(pr)
}



#--- Get combination of satellites ---

sat_tb <-
  sat_char |>
  tidyr::expand_grid(sat_char) |>
  magrittr::set_colnames(c("satelite_x", "satelite_y")) |>
  dplyr::filter(satelite_x != satelite_y)

stopifnot("Invalid number of columns!" = ncol(sat_tb) == 2)

# Append data from each satellite.
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



#---- Fit regressions models ----

sat_lm_tb <-
  sat_tb |>
  #NOTE: 01 month regressions start here.
  # Fit a linear model using overlapping x & y data.
  dplyr::mutate(
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
  # Compute models statistics r-squred and adjusted r-squared.
  dplyr::mutate(
    rsquared_01 = purrr::map(
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
          # NOTE: broom::tidy extracts data from the model as a tibble. It
          # doesn't make computations. So the confidence level reported by it
          # should be the same as computed by other means.
          conf.int = TRUE,
          conf.level = confidence_level
        ) |>
          purrr::list_rbind(names_to = "month")
      }
    )
  ) |>
  # Keep satellite combinations that have enough models (12).
  dplyr::mutate(
    n_models_01 = purrr::map_int(
      .x = lm_01_models,
      .f = length
    )
  ) |>
  # Predict future and past using the fitted models.
  dplyr::mutate(
    forecast_01_df = purrr::map2(
      .x = lm_01_models,
      .y = x_df,
      .f = function(lm_ls, x_df) {
        if(length(lm_ls) != 12){
          return(NA)
        }
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
  #NOTE: 12 month regressions start here.
  # Fit a linear model usin x & y data.
  dplyr::mutate(
    lm_data_month = purrr::map2(
      .x = x_df,
      .y = y_df,
      .f = fit_lm_12_months,
      formula = "y ~ x",
      clevel = confidence_level
    )
  ) |>
  dplyr::mutate(
    # Get the fitted models in their own column.
    lm_12_models = purrr::map(
      .x = lm_data_month,
      .f = function(data_ls) { return(data_ls[["model"]]) }
    ),
    # Get the fitted model data in a column.
    lm_12_data = purrr::map(
      .x = lm_data_month,
      .f = function(data_ls, data_names) { return(data_ls[["data"]]) }
    )
  ) |>
  dplyr::select(-lm_data_month) |>
  # Compute models statistics r squred and adjusted r squared.
  dplyr::mutate(
    rsquared_12 = purrr::map(
      .x = lm_12_models,
      .f = function(lm_model) {
        rsqr_tb <- data.frame(
          rsqr = get_lm_r2(lm_obj = lm_model, adjusted = FALSE),
          rsqr_adj = get_lm_r2(lm_obj = lm_model, adjusted = TRUE)
        )
        return(rsqr_tb)
      }
    )
  ) |>
  # Get the models parameters as a tibble.
  dplyr::mutate(
    lm_12_model_param = purrr::map(
      .x = lm_12_models,
      .f = function(model_ls) {
        return(broom::tidy(model_ls))
      }
    )
  ) |>
  # TODO: Create plots for the model in each row.
  # Keep satellite combinations that have enough models (12).
  dplyr::mutate(
    n_models_12 = purrr::map_int(
      .x = lm_12_models,
      .f = function(lm_model) {
          if (inherits(lm_model, what = "lm")) {
            return(1)
          }
          return(0)
      }
    )
  ) |>
  #dplyr::filter(n_models_12 == 1) |>
  # Predict future and past using the fitted models.
  dplyr::mutate(
    forecast_12_df = purrr::map2(
      .x = lm_12_models,
      .y = x_df,
      .f = function(lm_model, x_df) {
        if(inherits(x = lm_model, what = "lm") == FALSE){
          return(NA)
        }
        predict_mt <-
          predict_ci(
            lm_obj = lm_model,
            new_data = x_df[["n"]],
            clevel = confidence_level
          )
        predict_df <- as.data.frame(predict_mt)
        return(predict_df)
      }
    )
  )

# Save a table with regression parameters.

out_file <-
  file.path(
    out_dir,
    paste0(
      get_base_file_name(rds_file),
      "_lm.RDS"
    )
  )

sat_lm_tb |>
  dplyr::mutate(path_row = get_file_path_row(rds_file)) |>
  saveRDS(file = out_file)
