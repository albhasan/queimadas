#' Get a forecasting versus observations plot
#'
#' @description
#' Get a ggplot2 plot which compares a forecast versus a set of observations.
#'
#' @param obs_tb A data frame of observations.
#' @param forecast_tb a data frame of forecast values.
#'
#' @return A plot (ggplot2) object.
#'
get_plot_forecast_vs_obs <- function(obs_tb, forecast_tb) {
  stopifnot(
    "Missing columns in forecast data" =
      all(c(
        "ds", "yhat", "yhat_lower", "yhat_upper"
      ) %in% colnames(forecast_tb))
  )
  stopifnot(
    "Missing column in observation data" =
      all(c("satelite") %in% colnames(obs_tb))
  )
  ds <- satelite <- y <- yhat <- yhat_lower <- yhat_upper <- NULL
  fc_tb <-
    forecast_tb |>
    dplyr::filter(
      ds >= min(obs_tb[["ds"]]),
      ds <= max(obs_tb[["ds"]])
    )
  plot_fc_vs_obs <-
    ggplot2::ggplot() +
    ggplot2::geom_line(
      data = fc_tb,
      color = "blue",
      mapping = ggplot2::aes(
        x = ds,
        y = yhat
      )
    ) +
    ggplot2::geom_ribbon(
      data = fc_tb,
      mapping = ggplot2::aes(
        x = ds,
        y = yhat,
        ymin = yhat_lower,
        ymax = yhat_upper
      ),
      linetype = 2,
      alpha = 0.1
    ) +
    ggplot2::geom_point(
      data = obs_tb,
      mapping = ggplot2::aes(
        x = ds,
        y = y,
        group = satelite,
        color = satelite,
        shape = satelite
      )
    )
  return(plot_fc_vs_obs)
}



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
  ds <- SMA <- y <- NULL
  stopifnot(
    "Columns ds and y not found!" =
      all(c("ds", "y") %in% colnames(train_tb))
  )

  train_satellite <- paste(unique(train_tb[["satelite"]]), collapse = "-")

  plot_seasonality_smoothing <-
    train_tb |>
    dplyr::select(ds, y) |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = ds, y = y)) +
    ggplot2::geom_line() +
    tidyquant::geom_ma(
      ma_fun = SMA,
      n = 6,
      color = "blue"
    ) +
    ggplot2::labs(
      title = train_satellite,
      subtitle = "Do the data have seasonality after MA smoothing?",
      x = "Time",
      y = "Number of events"
    )

  plot_trend_smoothing <-
    train_tb |>
    dplyr::select(ds, y) |>
    timetk::plot_time_series(
      .date_var = ds,
      .value = y,
      .interactive = FALSE,
      .smooth = TRUE,
      .smooth_message = TRUE
    ) +
    ggplot2::labs(
      title = train_satellite,
      subtitle = "Do the data have a trend when smoothed?",
      x = "Time",
      y = "Number of events"
    )

  plot_trend_smoothing_log <-
    train_tb |>
    dplyr::select(ds, y) |>
    timetk::plot_time_series(
      .date_var = ds,
      .value = log(y),
      .interactive = FALSE,
      .smooth = TRUE,
      .smooth_message = TRUE
    ) +
    ggplot2::labs(
      title = train_satellite,
      subtitle = "Do the data have a trend after log(n) and smoothing?",
      x = "Time",
      y = "Number of events (log)"
    )

  return(list(
    plot_seasonality_smoothing = plot_seasonality_smoothing,
    plot_trend_smoothing = plot_trend_smoothing,
    plot_trend_smoothing_log = plot_trend_smoothing_log
  ))
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
  ds <- y <- NULL
  stopifnot(
    "Columns ds or y not found!" =
      c("ds", "y") %in% colnames(train_tb)
  )

  train_satelite <- paste(unique(train_tb[["satelite"]]), collapse = "-")

  fire_model <-
    train_tb |>
    dplyr::select(ds, y) |>
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
      subtitle = "Do the data have a trend component?",
      x = "Time",
      y = "Number of events"
    )

  plot_component_seasonality <-
    plot_components[[2]] +
    ggplot2::labs(
      title = train_satelite,
      subtitle = "Do the data have a seasonality component?",
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
  ds <- residual <- y <- yhat <- NULL
  fire_model <- fit_prophet_model(train_tb)
  fire_forecast <- stats::predict(fire_model)

  model_residual <-
    train_tb |>
    dplyr::select(ds, y) |>
    dplyr::left_join(
      y = dplyr::select(fire_forecast, ds, yhat),
      by = dplyr::join_by(ds)
    ) |>
    dplyr::mutate(
      residual = y - yhat
    )

  plot_residuals <-
    model_residual |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = yhat, y = residual)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(
      title = "Residual vs. Fitted Values Plot",
      x = "Fitted Values",
      y = "Residuals"
    )

  plot_residuals_hist <-
    model_residual |>
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
