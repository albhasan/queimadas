library(dplyr)
library(ggplot2)
library(logger)
library(purrr)
library(tibble)

library(queimadas)

#---- Configuration ----

sqlite_file <- "~/Documents/data/r_packages/queimadas/fire.sqlite"
table_name <- "fire_foci"
out_dir <- "/home/alber/Documents/github/slides/queimadas/slides"

plot_size_a5_ls <- get_paper_size(name = "A5", orientation = "ls")
plot_size_a4_ls <- get_paper_size(name = "A4", orientation = "ls")

confidence_level <- 0.95

#---- Get data from the database ----

db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)

brazil_ym_tb <-
  db_con |>
  get_brazil_year_month(table_name = table_name) |>
  dplyr::collect()

DBI::dbDisconnect(conn = db_con)
rm(db_con)


#---- Util ----

# Utilitary. Plot the regression lines of the month models.
# TODO: get_plot_lm_01 <- function(plot_data) {}

#---- Forecast using the queimadas approach ----

sat_char <- c(
  "AQUA_M-T",
  "NOAA-12",
  "NPP-375-PM",
  "NPP-375D"
)

# TODO: Add to figures!
# https://space.oscar.wmo.int/satellites/view/aqua
# https://space.oscar.wmo.int/satellites/view/noaa_12
# https://space.oscar.wmo.int/satellites/view/terra
# https://space.oscar.wmo.int/satellites/view/snpp
sat_metadata <- tibble::tribble(
  ~name,     ~launch,      ~elo,
  "AQUA",    "2002-05-04", ">2026",
  "TERRA",   "1999-12-18", ">2026",
  "NOAA-12", "1991-05-12", "2007-08-10",
  "SNPP",    "2011-10-28", ">2029"
)

stopifnot(
  "Reference satellite not found in data!" =
    all(sat_char %in% unique(brazil_ym_tb[["satelite"]]))
)

sat_tb <-
  sat_char |>
  tidyr::expand_grid(sat_char) |>
  magrittr::set_colnames(c("satelite_x", "satelite_y")) |>
  dplyr::filter(satelite_x != satelite_y)

stopifnot("Invalid number of columns!" = ncol(sat_tb) == 2)

sat_tb <-
  sat_tb |>
  # get all x & y data.
  dplyr::mutate(
    x_df = purrr::map(
      .x = satelite_x,
      .f = get_sat_data,
      data_df = brazil_ym_tb
    ),
    y_df = purrr::map(
      .x = satelite_y,
      .f = get_sat_data,
      data_df = brazil_ym_tb
    )
  ) |>
  dplyr::mutate(
    overlap_ts = purrr::map2_int(
      .x = x_df,
      .y = y_df,
      .f = overlap_len,
      cname = "period"
    )
  ) |>
  dplyr::filter(overlap_ts > 0) |>
  dplyr::select(-overlap_ts) |>
  # Fit a linear model using overlapping x & y data.
  dplyr::mutate(
    lm_data = purrr::map2(
      .x = x_df,
      .y = y_df,
      .f = fit_lm_12_months,
      formula = "y ~ x",
      clevel = confidence_level
    ),
    # Get the model.
    lm = purrr::map(
      .x = lm_data,
      .f = get_cdata,
      cname = "model"
    ),
    # Get the overlaping data.
    lm_data = purrr::map(
      .x = lm_data,
      .f = get_cdata,
      cname = "data"
    )
  ) |>
  # Predict future and past using the fitted model.
  dplyr::mutate(
    forecast_df = purrr::map2(
      .x = lm,
      .y = x_df,
      .f = function(lm_obj, data_vec) {
        pred_df <- predict_ci(
          lm_obj = lm_obj,
          new_data = data_vec[["n"]],
          clevel = confidence_level
        )
        pred_df <- cbind(data_vec["period"], pred_df)
        return(pred_df)
      }
    )
  ) |>
  # Plot the linear model between two reference satellites.
  dplyr::mutate(
    plot_lm = purrr::pmap(
      .l = list(
        x = satelite_x,
        y = satelite_y,
        data_df = lm_data,
        lm_obj = lm
      ),
      .f = get_plot_ref_sats
    )
  ) |>
  # Plot model prediction using the queimadas method.
  dplyr::mutate(
    plot_queimadas = purrr::pmap(
      .l = list(
        x_df = x_df,
        y_df = y_df,
        forecast_df = forecast_df
      ),
      .f = get_plot_queimadas_forecast
    )
  ) |>
  # Fit a linear model by month.
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
        forecast_df = forecast_df
      ),
      .f = get_plot_queimadas_forecast
    )
  ) |>
  # Estimate differeces between the queimadas methods' forecast.
  dplyr::mutate(
    forecast_diff = purrr::map2(
      .x = forecast_df,
      .y = forecast_01_df,
      .f = function(forecast_df, forecast_01_df) {
        cnames <- c("fit", "lwr", "upr")
        res <- forecast_df[, cnames] - forecast_01_df[, cnames]
        per <- forecast_df["period"]
        res <- cbind(per, res)
        colnames(res) <- c(
          colnames(res)[1],
          paste0(colnames(res)[-1], "_diff")
        )
        return(res)
      }
    )
  ) |>
  dplyr::mutate(
    plot_forecast_diff = purrr::map(
      .x = forecast_diff,
      .f = function(forecast_diff) {
        forecast_diff["period_date"] <- paste0(forecast_diff[["period"]], "-15")
        forecast_diff["period_date"] <- as.Date(forecast_diff[["period_date"]])
        ggplot2::ggplot(data = forecast_diff) +
          ggplot2::geom_ribbon(
            mapping = ggplot2::aes(
              x = period_date,
              ymin = lwr_diff,
              ymax = upr_diff
            ),
            fill = "gray80"
          ) +
          ggplot2::geom_line(
            mapping = ggplot2::aes(
              x = period_date,
              y = fit_diff
            )
          ) +
          ggplot2::geom_point(
            mapping = ggplot2::aes(
              x = period_date,
              y = fit_diff
            )
          ) +
          ggplot2::xlab("Time") +
          ggplot2::ylab("Difference")
      }
    )
  )

logger::log_info("Creating regression plots of the reference satellites...")

for (i in seq_len(nrow(sat_tb))) {
  sat_x <- sat_tb[["satelite_x"]][[i]]
  sat_y <- sat_tb[["satelite_y"]][[i]]
  lm_obj <- sat_tb[["lm"]][[i]]
  p <- sat_tb[["plot_lm"]][[i]]
  plot_file <- file.path(
    out_dir,
    "figures",
    paste0("plot_queimadas_lm_", sat_y, "_along_", sat_x, ".png")
  )
  logger::log_info("Saving linear model plot to file: ", basename(plot_file))
  ggplot2::ggsave(
    filename = plot_file,
    plot = p,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}

logger::log_info(
  "Creating regression plots (month-by-month) of the reference satellites..."
)

for (i in seq_len(nrow(sat_tb))) {
  sat_x <- sat_tb[["satelite_x"]][[i]]
  sat_y <- sat_tb[["satelite_y"]][[i]]
  p <- sat_tb[["plot_lm_01"]][[i]]
  plot_file <- file.path(
    out_dir,
    "figures",
    paste0("plot_queimadas_lm_01_", sat_y, "_along_", sat_x, ".png")
  )
  logger::log_info(
    "Saving linear model 01 plot to file: ",
    basename(plot_file)
  )
  ggplot2::ggsave(
    filename = plot_file,
    plot = p,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}


logger::log_info("Writing lm parameters to CSV...")

lm_param_tb <-
  sat_tb |>
  dplyr::mutate(
    equation = purrr::map_chr(
      .x = lm,
      .f = get_lm_equation
    ),
    r2 = purrr::map_chr(
      .x = lm,
      .f = function(x) {
        sprintf("%.2f", get_lm_r2(lm_obj = x))
      }
    )
  ) |>
  dplyr::select(satelite_x, satelite_y, equation, r2) |>
  get_latex_char() |>
  readr::write_csv(
    file = file.path(
      out_dir,
      "tables",
      "queimadas_method_lm_equation.csv"
    )
  )

logger::log_info("Writing lm outliers to CSV...")

lm_outliers_tb <-
  sat_tb |>
  dplyr::mutate(
    outliers_file = file.path(
      out_dir,
      "tables",
      paste0(
        "outliers_queimadas_lm_", satelite_y, "_along_", satelite_x, ".csv"
      )
    )
  ) |>
  dplyr::mutate(
    write_csv_file = purrr::map2_chr(
      .x = lm_data,
      .y = outliers_file,
      .f = function(lm_data, outliers_file) {
        lm_data |>
          dplyr::rename(outlier = is_outlier) |>
          dplyr::filter(outlier == TRUE) |>
          get_latex_char() |>
          readr::write_csv(
            file = outliers_file
          )
        return(outliers_file)
      }
    )
  )

logger::log_info("Creaing break lines on August of each year...")

break_lines <-
  get_break_lines_year(
    data_tb = brazil_ym_tb,
    period_col = "period",
    break_pattern = "-08$"
  ) |>
  period_to_date()

logger::log_info("Plot forecast using queimadas method...")

for (i in seq_len(nrow(sat_tb))) {
  sat_x <- sat_tb[["satelite_x"]][[i]]
  sat_y <- sat_tb[["satelite_y"]][[i]]
  p <-
    sat_tb[["plot_queimadas"]][[i]] +
    ggplot2::scale_x_continuous(breaks = break_lines) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Number of events") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    ) +
    ggplot2::labs(
      title = sprintf("Transformation of %s into %s", sat_x, sat_y),
      subtitle = sprintf(
        "Forecast (blue line) versus observations (%s red crosses, %s black plusses)",
        sat_y, sat_x
      )
    )
  plot_file <- file.path(
    out_dir,
    "figures",
    paste0("plot_forecast_", sat_y, "_along_", sat_x, ".png")
  )
  logger::log_info("Saving plot to file: ", basename(plot_file))
  ggplot2::ggsave(
    filename = plot_file,
    plot = p,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}

logger::log_info("Plot forecast using queimadas (1-month) method...")

for (i in seq_len(nrow(sat_tb))) {
  sat_x <- sat_tb[["satelite_x"]][[i]]
  sat_y <- sat_tb[["satelite_y"]][[i]]
  p <-
    sat_tb[["plot_queimadas_01"]][[i]] +
    ggplot2::scale_x_continuous(breaks = break_lines) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Number of events") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    ) +
    ggplot2::labs(
      title = sprintf("Transformation of %s into %s", sat_x, sat_y),
      subtitle = sprintf(
        "Forecast (blue line) versus observations (%s red crosses, %s black plusses)",
        sat_y, sat_x
      )
    )
  plot_file <- file.path(
    out_dir,
    "figures",
    paste0("plot_forecast_01_", sat_y, "_along_", sat_x, ".png")
  )
  logger::log_info("Saving plot to file: ", basename(plot_file))
  ggplot2::ggsave(
    filename = plot_file,
    plot = p,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}

logger::log_info(
  "Plot differences between queimadas methods (12 and 1 month)..."
)

for (i in seq_len(nrow(sat_tb))) {
  sat_x <- sat_tb[["satelite_x"]][[i]]
  sat_y <- sat_tb[["satelite_y"]][[i]]
  p <-
    sat_tb[["plot_forecast_diff"]][[i]] +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    ) +
    ggplot2::scale_x_continuous(breaks = break_lines) +
    ggplot2::labs(
      title = "Forecast differences using the Queimadas method 12 versus 1 month",
      subtitle = sprintf(
        "Differences in forecasts of %s using %s",
        sat_y,
        sat_x
      ),
      x = "Time",
      y = "Differentec in number of events"
    )
  plot_file <- file.path(
    out_dir,
    "figures",
    paste0(
      "plot_forecast_differences_12_vs_1_",
      sat_y,
      "_along_",
      sat_x,
      ".png"
    )
  )
  logger::log_info("Saving plot to file: ", basename(plot_file))
  ggplot2::ggsave(
    filename = plot_file,
    plot = p,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}

# TODO: Regression curve by region.

# no caso do aqua voltar ate 1998
# correlacao de pearson da regiao sul
# bfast? perguntar liana sobre o metodo que ela oleo
#
#
# comparaçao do brazil com regiao, como ficaram os residuais,

# TODO: Liana olhar segmented regression analysis, modified mamn-kentall, sem slope, loeless regression
