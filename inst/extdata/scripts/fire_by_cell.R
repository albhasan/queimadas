#!/usr/bin/env Rscript
###############################################################################
# RUN REGRESSIONS ON THE ACTIVE FIRES IN EACH CELL IN THE GRID
#------------------------------------------------------------------------------
# NOTE: The number of points used in each monthly regression is the number of
#       rows in the data extracted from the model.
#------------------------------------------------------------------------------
# ./fire_by_cell.R
# -f /home/alber/Documents/results/r_packages/queimadas/cell_ts/active-fires_233_067.RDS
# -o /home/alber/Documents/results/r_packages/queimadas/cell_lm
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
out_dir <- opt$out
rds_file <- opt$file

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
  dplyr::filter(overlap_ts > 0)



#---- Fit regressions models ----

sat_lm_tb <-
  sat_tb |>
  #note: 01 month regressions start here.
  # fit a linear model using overlapping x & y data.
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
    # get the fitted models in their own column.
    lm_01_models = purrr::map(
      .x = lm_data_month,
      .f = function(data_ls) {
        data_tb_ls <- lapply(X = data_ls, FUN = function(a) {
          if ("model" %in% names(a)) { 
            return(a[["model"]])
          }
          return(NA)
        })
      }
    )
  ) |>
  dplyr::mutate(
    # get the fitted model data in a column.
    lm_01_data = purrr::map(
      .x = lm_data_month,
      .f = function(data_ls) {
        data_tb_ls <-
          lapply(X = data_ls, FUN = function(a) {
            if(any(is.na(a))) {return(NA)}
            data_tb <- tibble::as_tibble(a[["data"]])
          })
        data_tb_ls <- data_tb_ls[is.na(data_tb_ls) == FALSE]
        return(dplyr::bind_rows(lnames2df(df_ls = data_tb_ls, cname = "month")))
      }
    )
  ) |>
  dplyr::select(-lm_data_month) |>
  # compute models statistics r-squred and adjusted r-squared.
  dplyr::mutate(
    rsquared_01 = purrr::map(
      .x = lm_01_models,
      .f = function(lm_models){
        rsqr_tb <-
          data.frame(
            rsqr = get_lm_r2(
              lm_obj = lm_models,
              adjusted = FALSE
            ),
            rsqr_adj = get_lm_r2(
              lm_obj = lm_models,
              adjusted = TRUE
            )
          )
        rsqr_tb <- tibble::rownames_to_column(rsqr_tb, var = "month")
        return(rsqr_tb)
      }
    )
  ) |>
  # get the models parameters as a tibble.
  dplyr::mutate(
    lm_01_model_param = purrr::map(
      .x = lm_01_models,
      .f = function(models_ls) {
        lm_tb <-
          purrr::map(
            .x = models_ls,
            .f = function(lm_obj) {
                if (length(lm_obj) == 1 && is.na(lm_obj)) {return(NA)}
                if (inherits(x = lm_obj, what = "lm")) {
                  return(broom::tidy(lm_obj))
                }
              }
          )
        lm_tb <- lm_tb[is.na(lm_tb) == FALSE]
        return(purrr::list_rbind(lm_tb, names_to = "month"))
      }
    )
  ) |>
  # count the number of monthly models for each par of satellites.
  dplyr::mutate(
    n_models_01 = purrr::map_int(
      .x = lm_01_models,
      .f = function(lm_models) {
        na_models <- 
          vapply(
            X = lm_models,
            FUN = function(a){return(length(a) == 1 && is.na(a))},
            FUN.VALUE = logical(1)
          )
        valid_models <- sum(na_models == FALSE)
        return(valid_models)
      }
    )
  ) |>
  # Predict future and past using the fitted models.
  dplyr::mutate(
    forecast_01_df = purrr::map2(
      .x = lm_01_models,
      .y = x_df,
      .f = function(lm_ls, x_df) {
        if(length(lm_ls) != 12){ return(NA) }
        if(sum(is.na(lm_ls)) != 0){ return(NA) }
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
  ) |>
  # Fill in forescast_df with the predictions in forescast_01_df. If missing,
  # fill it with the predictions in forescast-12_df.
  dplyr::mutate(
    forescast_df = dplyr::if_else(
      condition = is.na(forecast_01_df),
      true =  forecast_12_df,
      false = forecast_01_df,
      missing = NA
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
