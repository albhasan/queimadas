library(devtools)
devtools::load_all()

library(dplyr)
# library(forecast)
library(ggplot2)
library(logger)
library(purrr)
library(tibble)
# library(lubridate)

#---- Configuration ----

sqlite_file <- "~/Documents/data/r_packages/queimadas/fire.sqlite"
table_name <- "fire_foci"
out_dir <- "/home/alber/Documents/github/slides/queimadas/slides/figures"

plot_size_a5_ls <- get_paper_size(name = "A5", orientation = "ls")
plot_size_a4_ls <- get_paper_size(name = "A4", orientation = "ls")


#---- Get data from the database ----

db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)

brazil_ym_tb <-
  db_con |>
  get_brazil_year_month(table_name = table_name) |>
  dplyr::collect()

DBI::dbDisconnect(conn = db_con)
rm(db_con)


#---- Forescat using queimadas approach ----

# NOTE: Not all satelite combinations are valid!
sat_tb <-
  tibble::tribble(
    ~satelite_x, ~satelite_y,
    "AQUA_M-T", "NOAA-12",
    "AQUA_M-T", "NPP-375D",
    "AQUA_M-T", "NPP-375-PM"
  )

sat_tb <-
  sat_tb |>
  # Get all x & y data.
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
  # Fit a linear model using overlapping x & y data.
  dplyr::mutate(
    lm_data = purrr::map2(
      .x = x_df,
      .y = y_df,
      .f = fit_montly_lm
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
  # Predict the future and past using the fitted model.
  dplyr::mutate(
    forecast_df = purrr::map2(
      .x = lm,
      .y = x_df,
      .f = function(lmodel, data_vec) {
        new_data <- data_vec
        colnames(new_data)[2] <- "x"
        pred_df <-
          data.frame(
            period = data_vec[["period"]],
            n = stats::predict(
              object = lmodel,
              newdata = new_data["x"]
            )
          )
        return(pred_df)
      }
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
      .f = plot_queimadas_forecast
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
      .f = function(x, y, data_df, lm_obj) {
        yhat <- stats::predict(
          object = lm_obj,
          newdata = data_df["x"]
        )
        data_df["yhat"] <- yhat
        p <-
          ggplot2::ggplot() +
          ggplot2::geom_point(
            mapping = ggplot2::aes(
              x = data_df[["x"]],
              y = data_df[["y"]],
              color = data_df[["m"]]
            )
          ) +
          ggplot2::geom_line(
            mapping = ggplot2::aes(
              x = data_df[["x"]],
              y = data_df[["yhat"]]
            ),
            color = "blue"
          ) +
          ggplot2::geom_abline(
            intercept = 0,
            slope = 1,
            linetype = "dashed"
          ) +
          ggplot2::xlab(x) +
          ggplot2::ylab(y) +
          ggplot2::labs(colour = "")
      }
    )
  )


logger::log_info("Creating linear models' plots...")

for (i in seq_len(nrow(sat_tb))) {
  sat_x <- sat_tb[["satelite_x"]][[i]]
  sat_y <- sat_tb[["satelite_y"]][[i]]
  lm_obj <- sat_tb[["lm"]][[i]]
  p <- sat_tb[["plot_lm"]][[i]]
  ggplot2::ggsave(
    filename = file.path(
      out_dir, paste0("plot_queimadas_lm_", sat_y, "_along_", sat_x, ".png")
    ),
    plot = p,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}

logger::log_info("Creaing break lines on August of each year...")
break_lines <-
  get_break_lines_year(
    data_tb = brazil_ym_tb,
    period_col = "period",
    break_pattern = "-08$"
  )

logger::log_info("Writing lm parameters to CSV...")
lm_param_tb <-
  sat_tb |>
  dplyr::mutate(
    equation = purrr::map_chr(
      .x = lm,
      .f = get_lm_equation
    )
  ) |>
  dplyr::select(satelite_x, satelite_y, equation) |>
  get_latex_char() |>
  readr::write_csv(
    file = file.path(
      dirname(out_dir), "tables", "queimadas_method_lm_equation.csv"
    )
  )


logger::log_info("Plot forecast using queimadas method...")
for (i in seq_len(nrow(sat_tb))) {
  sat_x <- sat_tb[["satelite_x"]][[i]]
  sat_y <- sat_tb[["satelite_y"]][[i]]
  p <-
    sat_tb[["plot_queimadas"]][[i]] +
    ggplot2::scale_x_discrete(breaks = break_lines) +
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

  ggplot2::ggsave(
    filename = file.path(
      out_dir, paste0("plot_forecast_", sat_y, "_along_", sat_x, ".png")
    ),
    plot = p,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}


# TODO: compute residuals and plot them, and add them to the slides.
# TODO: Regression curve by region.

# no caso do aqua voltar ate 1998
# observaçoes com a metodologia de uma serie temporal
# correlacao de pearson da regiao sul
# bfast? perguntar liana sobre o metodo que ela oleo
#
# reumiao semana do 9 a 13
#
# comparaççao do brazil com regiao, como ficaram os residuais,
# \liaja consigue o dia 23

# TODO: Liana olhar segmented regression analysis, modified mamn-kentall, sem slope, loeless regression
