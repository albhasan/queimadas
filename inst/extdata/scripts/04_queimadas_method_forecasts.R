library(devtools)
devtools::load_all()

library(dplyr)
library(ggplot2)
library(logger)
library(purrr)
library(tibble)

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


#---- Forecast using the queimadas approach ----


sat_char <- c(
  "AQUA_M-T",
  "NOAA-12",
  "NPP-375-PM",
  "NPP-375D"
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
      .f = fit_monthly_lm,
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


# TODO: Regression curve by region.

# no caso do aqua voltar ate 1998
# correlacao de pearson da regiao sul
# bfast? perguntar liana sobre o metodo que ela oleo
#
#
# comparaçao do brazil com regiao, como ficaram os residuais,

# TODO: Liana olhar segmented regression analysis, modified mamn-kentall, sem slope, loeless regression
