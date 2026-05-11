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


#' Plot the results of the Queimadas method
#'
#' @description
#' Plot the results of applying the Queimadas method. This method consists on
#' finding the time overlap between two time series of observations and
#' adjust a linear model using the montly, but not the yearly, aggregated data.
#' Then, the results of this allows to obtain for each `x` observation the
#' estimatd `y` value for the time extent of `x`.
#'
#' @param x_df a data frame. The data on which `y` would be estimated.
#' @param y_df a data frame. The data used to fit a linear model on the
#' temporal overlaping with `x_df`
#' @param forecast_df a data frame. The results of projecting y for the time
#' extent of x.
#'
#' @return a plot object (ggplot2).
#'
#' @export
#'
get_plot_queimadas_forecast <- function(x_df, y_df, forecast_df) {
  fit <- lwr <- period_date <- upr <- n <- NULL

  f_line_color <- "blue"
  f_line_type <- "solid"
  f_line_width <- 0.4
  f_point_color <- "blue"
  f_point_shape <- "circle"
  f_point_size <- 2

  x_line_color <- "black"
  x_line_width <- 0.2
  x_line_type <- "dotted"
  x_point_color <- "black"
  x_point_shape <- "plus"
  x_point_size <- 1

  y_line_color <- "red"
  y_line_width <- 0.2
  y_line_type <- "dashed"

  y_point_shape <- "cross"
  y_point_color <- "red"
  y_point_size <- 1

  forecast_df <- forecast_df[order(forecast_df[["period"]]), ]
  x_df <- x_df[order(x_df[["period"]]), ]
  y_df <- y_df[order(y_df[["period"]]), ]

  forecast_df["period_date"] <- period_to_date(forecast_df[["period"]])
  x_df["period_date"] <- period_to_date(x_df[["period"]])
  y_df["period_date"] <- period_to_date(y_df[["period"]])

  p <-
    ggplot2::ggplot() +
    # Shadow: confidence interval.
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        x = period_date,
        ymin = lwr,
        ymax = upr,
      ),
      fill = "gray80",
      data = forecast_df
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = period_date,
        y = fit,
        group = 1
      ),
      color = f_line_color,
      linewidth = f_line_width,
      linetype = f_line_type,
      data = forecast_df
    ) +
    # Forecast points.
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = period_date,
        y = fit
      ),
      color = f_point_color,
      shape = f_point_shape,
      size = f_point_size,
      data = forecast_df
    ) +
    # X line.
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = period_date,
        y = n,
        group = 1
      ),
      color = x_line_color,
      linewidth = x_line_width,
      linetype = x_line_type,
      data = x_df
    ) +
    # X points.
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = period_date,
        y = n
      ),
      color = x_point_color,
      shape = x_point_shape,
      size = x_point_size,
      data = x_df
    ) +
    # Y line.
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = period_date,
        y = n,
        group = 1
      ),
      color = y_line_color,
      linewidth = y_line_width,
      linetype = y_line_type,
      data = y_df
    ) +
    # Y points.
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = period_date,
        y = n
      ),
      color = y_point_color,
      shape = y_point_shape,
      size = y_point_size,
      data = y_df
    )

  return(p)
}


#' Get a regression plot between reference satellites
#'
#' @description
#' Get a ggplot2 plot which compares a reference satellite observations.
#'
#' @param x,y a character(1). Name of a reference satellite.
#' @param data_df a data frame with overlapping observations of reference
#' satellites.
#' @param lm_obj a regression object (see `stats::lm`).
#'
#' @return A plot (ggplot2) object.
#'
get_plot_ref_sats <- function(x, y, data_df, lm_obj) {
  fit <- is_outlier <- lwr <- m <- upr <- NULL
  stopifnot(
    "Required columns not found!" =
      c("fit", "is_outlier", "lwr", "m", "upr") %in% colnames(data_df)
  )
  p <-
    ggplot2::ggplot(data = data_df) +
    # Shadow: Confidence interval.
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        x = x,
        ymin = lwr,
        ymax = upr,
      ),
      fill = "gray80"
    ) +
    # Points: y ~ x.
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = x,
        y = y,
        color = m,
        shape = is_outlier
      )
    ) +
    # Line: yhat ~ x.
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = x,
        y = fit
      ),
      color = "blue"
    ) +
    # Line y = x.
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"
    ) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(
        x = range(x)[1],
        y = range(y)[2],
        hjust = 0,
        vjust = 1,
        label = sprintf(
          "%s\nr2 = %s",
          get_lm_equation(lm_obj, dig = 2),
          round(get_lm_r2(lm_obj), digits = 2)
        ),
        parse = TRUE
      )
    ) +
    ggplot2::xlab(x) +
    ggplot2::ylab(y) +
    ggplot2::labs(
      colour = "Month",
      shape = "Outlier"
    )

  return(p)
}


#' Get a regression plot by month between reference satellites
#'
#' @description
#' Get a ggplot2 plot which compares a reference satellite observations in a
#' month-by-month basis.
#'
#' @param x,y a character(1). Name of a reference satellite.
#' @param data_df a data frame with overlapping observations of reference
#' satellites.
#'
#' @return A plot (ggplot2) object.
#'
get_plot_ref_sats_01 <- function(x, y, data_df) {
  fit <- lwr <- m <- upr <- month <- NULL
  stopifnot(
    "Required columns not found!" =
      c("fit", "lwr", "m", "upr", "month") %in% colnames(data_df)
  )

  p <-
    data_df |>
    ggplot2::ggplot() +
    # Shadow: Confidence interval.
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        x = x,
        ymin = lwr,
        ymax = upr,
        fill = month,
        group = month
      ),
      alpha = 0.2
    ) +
    # Line yhat ~ x.
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = x,
        y = fit,
        colour = month,
        group = month
      )
    ) +
    # Points x ~ y.
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = x,
        y = y,
        colour = month,
        group = month
      )
    ) +
    # Line y = x.
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"
    ) +
    ggplot2::xlab(x) +
    ggplot2::ylab(y)

  return(p)
}



# get_plot_queimadas_forecast_01 <- function(x_df, y_df, forecast_df) {
#   fit <- lwr <- period_date <- upr <- n <- NULL
#
#   f_line_color <- "blue"
#   f_line_type <- "solid"
#   f_line_width <- 0.4
#   f_point_color <- "blue"
#   f_point_shape <- "circle"
#   f_point_size <- 2
#
#   x_line_color <- "black"
#   x_line_width <- 0.2
#   x_line_type <- "dotted"
#   x_point_color <- "black"
#   x_point_shape <- "plus"
#   x_point_size <- 1
#
#   y_line_color <- "red"
#   y_line_width <- 0.2
#   y_line_type <- "dashed"
#
#   y_point_shape <- "cross"
#   y_point_color <- "red"
#   y_point_size <- 1
#
#   forecast_df <- forecast_df[order(forecast_df[["period"]]), ]
#   x_df <- x_df[order(x_df[["period"]]), ]
#   y_df <- y_df[order(y_df[["period"]]), ]
#
#   forecast_df["period_date"] <- period_to_date(forecast_df[["period"]])
#   x_df["period_date"] <- period_to_date(x_df[["period"]])
#   y_df["period_date"] <- period_to_date(y_df[["period"]])
#
#   p <-
#     ggplot2::ggplot() +
#     # Shadow: confidence interval.
#     ggplot2::geom_ribbon(
#       mapping = ggplot2::aes(
#         x = period_date,
#         ymin = lwr,
#         ymax = upr,
#       ),
#       fill = "gray80",
#       data = forecast_df
#     ) +
#     # Forecast line.
#     ggplot2::geom_line(
#       mapping = ggplot2::aes(
#         x = period_date,
#         y = fit,
#         group = 1
#       ),
#       color = f_line_color,
#       linewidth = f_line_width,
#       linetype = f_line_type,
#       data = forecast_df
#     ) +
#     # Forecast points.
#     ggplot2::geom_point(
#       mapping = ggplot2::aes(
#         x = period_date,
#         y = fit
#       ),
#       color = f_point_color,
#       shape = f_point_shape,
#       size = f_point_size,
#       data = forecast_df
#     ) +
#     # X line.
#     ggplot2::geom_line(
#       mapping = ggplot2::aes(
#         x = period_date,
#         y = n,
#         group = 1
#       ),
#       color = x_line_color,
#       linewidth = x_line_width,
#       linetype = x_line_type,
#       data = x_df
#     ) +
#     # X points.
#     ggplot2::geom_point(
#       mapping = ggplot2::aes(
#         x = period_date,
#         y = n
#       ),
#       color = x_point_color,
#       shape = x_point_shape,
#       size = x_point_size,
#       data = x_df
#     ) +
#     # Y line.
#     ggplot2::geom_line(
#       mapping = ggplot2::aes(
#         x = period_date,
#         y = n,
#         group = 1
#       ),
#       color = y_line_color,
#       linewidth = y_line_width,
#       linetype = y_line_type,
#       data = y_df
#     ) +
#     # Y points.
#     ggplot2::geom_point(
#       mapping = ggplot2::aes(
#         x = period_date,
#         y = n
#       ),
#       color = y_point_color,
#       shape = y_point_shape,
#       size = y_point_size,
#       data = y_df
#     )
#
#   return(p)
# }
