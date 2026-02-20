library(devtools)
devtools::load_all()

library(dplyr)
library(forecast)
library(tibble)
library(lubridate)

#library(httpgd)
#httpgd::hgd()



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



#' Visual test of time series
#'
#' @description
#' Produce figures that help diagnosing time series for trend and seasonality.
#'
#' @param train_tb a tibble with time series data.
#'
#' @return a list of ggplot2 objects.
#'
visual_test_ts <- function(train_tb) {
  stopifnot("Columns ds and y not found!" = 
            all(c("ds", "y") %in% colnames(train_tb)))

  train_satellite <- paste(unique(train_tb[["satelite"]]), collapse = "-")

  plot_seasonality_smoothing <-
    train_tb %>%
    dplyr::select(ds, y) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = ds, y = y)) +
    ggplot2::geom_line() +
    tidyquant::geom_ma(
      ma_fun = SMA,
      n = 6,
      color = "blue"
    ) +
    ggplot2::labs(
      title  = train_satellite,
      subtitle = "Does the data have seasonality after MA smoothing?",
      x = "Time",
      y = "Number of events"
    )

  plot_trend_smoothing <-
    train_tb %>%
    dplyr::select(ds, y) %>%
    timetk::plot_time_series(
      .date_var = ds,
      .value = y,
      .interactive = FALSE,
      .smooth = TRUE,
      .smooth_message = TRUE
    ) +
    ggplot2::labs(
      title = train_satellite,
      subtitle = "Does the data have a trend when smoothed?",
      x = "Time",
      y = "Number of events"
    )

  plot_trend_smoothing_log <-
    train_tb %>%
    dplyr::select(ds, y) %>%
    timetk::plot_time_series(
      .date_var = ds,
      .value = log(y),
      .interactive = FALSE,
      .smooth = TRUE,
      .smooth_message = TRUE
    ) +
    ggplot2::labs(
      title  = train_satellite,
      subtitle = "Does the data have a trend after log(n) and smoothing?",
      x = "Time",
      y = "Number of events (log)"
    )

  return(list(
    plot_seasonality_smoothing = plot_seasonality_smoothing,
    plot_trend_smoothing = plot_trend_smoothing,
    plot_trend_smoothing_log = plot_trend_smoothing_log
  ))
}


#' Fit a prophet model
#'
#' @description
#' Fit a model to the data using the `prophet` package.
#'
#' @param train_tb a tibble with time series data.
#'
#' @return a model object (prophet).
#'
fit_prophet_model <- function(train_tb) {
  stopifnot(c("ds", "y") %in% colnames(train_tb))
  train_tb %>%
    dplyr::select(ds, y) %>%
    prophet::prophet(
      growth = "linear",
      yearly.seasonality = TRUE,
      weekly.seasonality = FALSE,
      daily.seasonality = FALSE,
      seasonality.mode = "additive"
    ) %>%
    return()
}

#' Component analysis of time series
#'
#' @description
#' Produce figures that help diagnosing time series by their components.
#'
#' @param train_tb a tibble with time series data.
#'
#' @return a list of ggplot2 objects.
#'
component_analysis <- function(train_tb) {
  stopifnot("Columns ds or y not found!" =
            c("ds", "y") %in% colnames(train_tb))

  train_satelite <- paste(unique(train_tb[["satelite"]]), collapse = "-")

  fire_model <-
    train_tb %>%
    dplyr::select(ds, y) %>%
    fit_prophet_model()

  fire_forecast <- stats::predict(fire_model)

  plot_components <-
    prophet::prophet_plot_components(
      m = fire_model,
      fcst = fire_forecast,
      uncertainty = TRUE
    )

  plot_component_trend <-
    plot_components[[1]] +
    ggplot2::labs(
      title = train_satelite,
      subtitle = "Does the data have a trend?",
      x = "Time",
      y = "Number of events"
    )

  plot_component_seasonality <-
    plot_components[[2]] +
    ggplot2::labs(
      title = train_satelite,
      subtitle = "Does the data have a seasonality?",
      x = "Time",
      y = "Number of events"
    )

  plot_model_forecast <-
    plot(fire_model, fire_forecast) +
    ggplot2::labs(
      title = train_satelite,
      subtitle = "Observations (points) versus forecasting (line)",
      x = "Time",
      y = "Number of events"
    )

  return(list(
    plot_component_trend = plot_component_trend,
    plot_component_seasonality = plot_component_seasonality,
    plot_model_forecast = plot_model_forecast
  ))
}



#' Test if the model residuals are normal
#'
#' @description
#' Get the distribution of model residuals to check if the follow a normal
#' distribution.
#'
#' @param train_tb a tibble with time series data.
#'
#' @return a list of ggplot2 objects.
#'
test_normal_residuals <- function(train_tb) {

  fire_model <- fit_prophet_model(train_tb) 
  fire_forecast <- stats::predict(fire_model)

  model_residual <-
    train_tb %>%
    dplyr::select(ds, y) %>%
    dplyr::left_join(
      y = dplyr::select(fire_forecast, ds, yhat),
      by = dplyr::join_by(ds)
    ) %>%
    dplyr::mutate(
      residual = y - yhat
    )

  plot_residuals <-
    model_residual %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = yhat, y = residual)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(
      title = "Residual vs. Fitted Values Plot",
      x = "Fitted Values",
      y = "Residuals"
    )

  plot_residuals_hist <-
    model_residual %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = residual)) +
    ggplot2::geom_histogram(
      fill = "steelblue",
      color = "black",
      bins = 30
    ) +
    ggplot2::labs(
      title = "Histogram of Residuals",
      subtitle = "Do the residuals have a normal distribution?",
      x = "Residuals",
      y = "Frequency"
    )

    return(list(
      plot_residuals = plot_residuals,
      plot_residuals_hist = plot_residuals_hist
    ))

}



#---- Database connection ----



db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)

brazil_ym_tb <-
  db_con %>%
  get_brazil_year_month (table_name = table_name) %>%
  dplyr::filter(satelite %in% ref_satellite) %>%
  dplyr::collect()

DBI::dbDisconnect(conn = db_con)
rm(db_con)

brazil_ym_tb <-
  brazil_ym_tb %>%
  dplyr::mutate(
    date = paste0(period, "-15"),
    date = lubridate::as_date(date)
  ) %>%
  dplyr::select(ds = date, satelite, y = n)

time_points <-
  brazil_ym_tb %>%
  dplyr::select(ds)

# Split data into tibbles by satelite.
data_tb_ls <-
  brazil_ym_tb %>%
  dplyr::arrange(ds) %>%
  dplyr::group_split(satelite)

sat_names <-
  sapply(data_tb_ls, FUN = function(x){return(unique(x$satelite)[1])})

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
  for (p_name in names(plot_ls))
    # print(plot_ls[[p_name]])
    ggplot2::ggsave(
      filename = file.path(out_dir, paste0(p_name, "_", x_satellite, ".png")),
      plot = plot_ls[[p_name]],
      width = plot_size_a5_ls[["width"]],
      height = plot_size_a5_ls[["height"]],
      units = plot_size_a5_ls[["units"]]
    )
}



#---- Test if the residuals have a normal distribution ----

for (x in satellite_tb[["data"]]) {
  plot_ls <- test_normal_residuals(train_tb = x)
  x_satellite <- paste(unique(x[["satelite"]]), collapse = "-")
  for (p_name in names(plot_ls))
    #print(plot_ls[[p_name]])
    ggplot2::ggsave(
      filename = file.path(out_dir, paste0(p_name, "_", x_satellite, ".png")),
      plot = plot_ls[[p_name]],
      width = plot_size_a5_ls[["width"]],
      height = plot_size_a5_ls[["height"]],
      units = plot_size_a5_ls[["units"]]
    )
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
      title  = x_satellite,
      subtitle = "Is the time series autocorrelated?",
      x = "Time",
      y = "Number of events"
    )
  #print(p)
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
  satellite_tb %>%
  dplyr::mutate(
    fire_model = purrr::map(
      .x = data,
      .f = fit_prophet_model
    )
  )

satellite_tb <-
  satellite_tb %>%
  dplyr::mutate(
    fire_prediction = purrr::map(
      .x = fire_model,
      .f = stats::predict,
      time_points
    )
  )

for (r in seq(nrow(satellite_tb))) {
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
    filename = file.path(out_dir, paste0("plot_forecast", "_", x_satellite, ".png")),
    plot = p,
    width = plot_size_a5_ls[["width"]],
    height = plot_size_a5_ls[["height"]],
    units = plot_size_a5_ls[["units"]]
  )
}



#---- TODO: Compare ----
#TODO: Compute the residuals by transforming one time series into another and
# compare the results to the forescast. Does this make sense?
