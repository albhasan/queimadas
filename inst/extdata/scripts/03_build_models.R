library(devtools)
devtools::load_all()

library(dplyr)
library(forecast)
library(purrr)
library(tibble)
library(lubridate)

# library(httpgd)
# httpgd::hgd()


#---- Configuration ----

sqlite_file <- "~/Downloads/fire.sqlite"
table_name <- "fire_foci"
out_dir <- "/home/alber/Documents/github/slides/queimadas/slides/figures"

ref_satellite <- c(
  previous = "NOAA-12",
  current = "AQUA_M-T",
  candidate = "NPP-375-M",
  candidate = "NPP-375-T",
  candidate = "NPP-375-PM",
  candidate = "NPP-375",
  candidate = "NPP-375D"
)

stopifnot("Database file not found!" = file.exists(sqlite_file))
stopifnot("Directory not found!" = dir.exists(out_dir))

plot_size_a5_ls <- queimadas::get_paper_size(name = "A5", orientation = "ls")
plot_size_a4_ls <- queimadas::get_paper_size(name = "A4", orientation = "ls")


#---- Utilitary functions ----


#---- Database connection ----


db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)

brazil_ym_tb <-
  db_con |>
  get_brazil_year_month(table_name = table_name) |>
  dplyr::filter(satelite %in% ref_satellite) |>
  dplyr::collect()

DBI::dbDisconnect(conn = db_con)
rm(db_con)

brazil_ym_tb <-
  brazil_ym_tb |>
  dplyr::mutate(
    date = paste0(period, "-15"),
    date = lubridate::as_date(date)
  ) |>
  dplyr::select(ds = date, satelite, y = n)

time_points <-
  brazil_ym_tb |>
  dplyr::select(ds)

# Split data into tibbles by satelite.
data_tb_ls <-
  brazil_ym_tb |>
  dplyr::arrange(ds) |>
  dplyr::group_split(satelite)

sat_names <-
  sapply(data_tb_ls, FUN = function(x) {
    return(unique(x$satelite)[1])
  })

satellite_tb <-
  tibble::tibble(
    name = sat_names,
    data = data_tb_ls
  )

rm(sat_names)
rm(data_tb_ls)


#---- Test for trend and seasonality ----


# Visual test
for (x in satellite_tb[["data"]]) {
  x_satellite <- paste(unique(x[["satelite"]]), collapse = "-")
  plot_ls <- visual_test_ts(train_tb = x)
  for (p_name in names(plot_ls)) {
    print(plot_ls[[p_name]])
    ggplot2::ggsave(
      filename = file.path(out_dir, paste0(p_name, "_", x_satellite, ".png")),
      plot = plot_ls[[p_name]],
      width = plot_size_a5_ls[["width"]],
      height = plot_size_a5_ls[["height"]],
      units = plot_size_a5_ls[["units"]]
    )
  }
}


#---- Component analysis ----

for (x in satellite_tb[["data"]]) {
  x_satellite <- paste(unique(x[["satelite"]]), collapse = "-")
  plot_ls <- component_analysis(train_tb = x)
  for (p_name in names(plot_ls)) {
    # print(plot_ls[[p_name]])
    ggplot2::ggsave(
      filename = file.path(out_dir, paste0(p_name, "_", x_satellite, ".png")),
      plot = plot_ls[[p_name]],
      width = plot_size_a5_ls[["width"]],
      height = plot_size_a5_ls[["height"]],
      units = plot_size_a5_ls[["units"]]
    )
  }
}


#---- Test if the residuals have a normal distribution ----

for (x in satellite_tb[["data"]]) {
  plot_ls <- test_normal_residuals(train_tb = x)
  x_satellite <- paste(unique(x[["satelite"]]), collapse = "-")
  for (p_name in names(plot_ls)) {
    # print(plot_ls[[p_name]])
    ggplot2::ggsave(
      filename = file.path(out_dir, paste0(p_name, "_", x_satellite, ".png")),
      plot = plot_ls[[p_name]],
      width = plot_size_a5_ls[["width"]],
      height = plot_size_a5_ls[["height"]],
      units = plot_size_a5_ls[["units"]]
    )
  }
}


#---- Autocorrelation ----

for (x in satellite_tb[["data"]]) {
  x_satellite <- paste(unique(x[["satelite"]]), collapse = "-")
  p <-
    forecast::ggAcf(
      x = x[["y"]],
      lag.max = NULL,
      type = "correlation",
      plot = TRUE,
      na.action = na.contiguous,
      demean = TRUE
    ) +
    ggplot2::labs(
      title = x_satellite,
      subtitle = "Is the time series autocorrelated?",
      x = "Time",
      y = "Number of events"
    )
  # print(p)
  ggplot2::ggsave(
    filename = file.path(
      out_dir,
      paste0("plot_autocorrelation", "_", x_satellite, ".png")
    ),
    plot = p,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}


#---- Forecast ----

satellite_tb <-
  satellite_tb |>
  dplyr::mutate(
    fire_model = purrr::map(
      .x = data,
      .f = fit_prophet_model
    )
  )

satellite_tb <-
  satellite_tb |>
  dplyr::mutate(
    fire_prediction = purrr::map(
      .x = fire_model,
      .f = stats::predict,
      time_points
    )
  )

for (r in seq_len(nrow(satellite_tb))) {
  x_satellite <- satellite_tb[[r, "name"]]
  p <-
    plot(
      satellite_tb$fire_model[[r]],
      satellite_tb$fire_prediction[[r]]
    ) +
    ggplot2::labs(
      title = x_satellite,
      subtitle = "Model (lines) for the whole time length",
      x = "Time",
      y = "Number of events"
    )
  print(p)
  ggplot2::ggsave(
    filename = file.path(
      out_dir, paste0("plot_forecast", "_", x_satellite, ".png")
    ),
    plot = p,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}


#---- TODO: Compare ----
# TODO: Compute the residuals by transforming one time series into another and
# compare the results to the forescast. Does this make sense?

# NOTE: The best looking forecast belongs to NPP-375-PM!

ref_fc <- "NPP-375-PM"

forecast_tb <-
  satellite_tb |>
  dplyr::filter(name == ref_fc) |>
  dplyr::pull(fire_prediction) |>
  dplyr::first() |>
  dplyr::select(ds, yhat, yhat_upper, yhat_lower)

obs_tb <-
  satellite_tb |>
  dplyr::select(data) |>
  tidyr::unnest(data)

plot_fc_vs_obs <-
  get_plot_forecast_vs_obs(
    obs_tb = obs_tb,
    forecast_tb = forecast_tb
  ) +
  ggplot2::labs(
    title = "Forescast versus Observations",
    subtitle = paste(
      "Forecast of",
      ref_fc,
      "(line) versus observations (points)"
    ),
    x = "Time",
    y = "Number of events"
  )

ggplot2::ggsave(
  filename = file.path(
    out_dir, paste0("plot_fc_", ref_fc, "_vs_obs.png")
  ),
  plot = plot_fc_vs_obs,
  width = plot_size_a5_ls[["width"]],
  height = plot_size_a5_ls[["height"]],
  units = plot_size_a5_ls[["units"]]
)

satellite_tb <-
  satellite_tb |>
  dplyr::mutate(
    plot_fc_vs_obs = purrr::map(
      .x = data,
      .f = get_plot_forecast_vs_obs,
      forecast_tb = forecast_tb
    )
  )

for (x_satellite in satellite_tb[["name"]]) {
  plot_fc_vs_obs <-
    satellite_tb |>
    dplyr::filter(name == x_satellite) |>
    dplyr::pull(plot_fc_vs_obs) |>
    dplyr::first()
  plot_fc_vs_obs <-
    plot_fc_vs_obs +
    ggplot2::labs(
      title = "Forescast versus Observations",
      subtitle = paste(
        "Forecast of",
        ref_fc,
        "(line) versus observations ",
        x_satellite,
        "(points)"
      ),
      x = "Time",
      y = "Number of events"
    )
  ggplot2::ggsave(
    filename = file.path(
      out_dir, paste0("plot_fc_", ref_fc, "_vs_obs_", x_satellite, ".png")
    ),
    plot = plot_fc_vs_obs,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}

get_fc_obs_residual <- function(obs_tb, forecast_tb) {
  ds <- residual <- y <- yhat <- NULL
  res <-
    obs_tb |>
    dplyr::left_join(
      y = forecast_tb,
      by = "ds"
    ) |>
    dplyr::mutate(
      residual = y - yhat
    ) |>
    dplyr::select(ds, residual)
  return(res)
}

satellite_tb <-
  satellite_tb |>
  dplyr::mutate(
    fc_obs_residual = purrr::map(
      .x = data,
      .f = get_fc_obs_residual,
      forecast_tb = forecast_tb
    )
  )

for (x_satellite in satellite_tb[["name"]]) {
  plot_res_obs_fc <-
    satellite_tb |>
    dplyr::filter(name == x_satellite) |>
    dplyr::pull(fc_obs_residual) |>
    dplyr::first() |>
    ggplot2::ggplot() +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = ds,
        y = residual
      )
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = ds,
        y = residual
      )
    ) +
    ggplot2::labs(
      title = "Forescast versus Observations (residuals)",
      subtitle = paste(
        "Residuals of",
        x_satellite,
        "minus the forecast of",
        ref_fc
      ),
      x = "Time",
      y = "Residual"
    )
  ggplot2::ggsave(
    filename = file.path(
      out_dir, paste0("plot_res_obs_", x_satellite, "_vs_fc_", ref_fc, ".png")
    ),
    plot = plot_res_obs_fc,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}
