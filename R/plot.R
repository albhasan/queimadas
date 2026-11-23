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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' Plot the results of applying the Queimadas method.
#'
#' @param x_df a data frame. The data on which `y` would be estimated.
#' @param y_df a data frame. The data used to fit a linear model on the
#' temporal overlaping with `x_df`.
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
#' @param use_adj_rsqr a logical(1). Should we add the adjusted R square to the
#' plot? Otherwise, add R squared.
#'
#' @return A plot (ggplot2) object.
#'
#' @export
#'
get_plot_ref_sats <- function(x, y, data_df, lm_obj, use_adj_rsqr) {
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
          round(get_lm_r2(lm_obj, adjusted = use_adj_rsqr), digits = 2)
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
#' @export
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
    ggplot2::ylab(y) +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  return(p)
}



#' Get a plot showing BFast breaks
#'
#' @description
#' Plot times series of queimadas data showing their break points.
#'
#' @param bfast_ts_tb A tibble with the time series, including a logical column
#' (*after_break*) representing the observations before and after the break.
#' @param bfast_top_tb A tibble with only the top observations of each period
#' (year).
#' @param break_lines A Date. Dates on which the perio changes (years).
#'
#' @return A ggplot2 object.
#'
#' @export
#'
get_plot_ts_with_bfast_break <- function(bfast_ts_tb, bfast_top_tb, break_lines) {
  after_break <- fit <- lwr <- n <- upr <- ymd <- NULL

  stopifnot(
    "Only one satellite allowed!" =
      length(unique(bfast_ts_tb[["satelite"]])) == 1
  )
  stopifnot(
    "Column after_break not found in bfast_ts_tb!" =
      "after_break" %in% colnames(bfast_ts_tb)
  )
  stopifnot(
    "Columns missing in bfast_top_tb!" =
      c("after_break", "lwr", "upr") %in% colnames(bfast_top_tb)
  )

  p <-
    ggplot2::ggplot() +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = ymd,
        y = n
      ),
      data = bfast_ts_tb,
      color = "blue"
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = ymd,
        y = n
      ),
      data = bfast_ts_tb,
      color = "blue",
      shape = 16
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = ymd,
        y = fit
      ),
      data = dplyr::filter(bfast_top_tb, after_break == FALSE),
      color = "black"
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = ymd,
        y = fit
      ),
      data = dplyr::filter(bfast_top_tb, after_break == FALSE),
      color = "black",
      shape = 0
    ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        x = ymd,
        ymin = lwr,
        ymax = upr,
        alpha = 0.9
      ),
      data = dplyr::filter(bfast_top_tb, after_break == FALSE),
      fill = "gray"
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = ymd,
        y = fit
      ),
      data = dplyr::filter(bfast_top_tb, after_break == TRUE),
      color = "black"
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = ymd,
        y = fit
      ),
      data = dplyr::filter(bfast_top_tb, after_break == TRUE),
      color = "black",
      shape = 0
    ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        x = ymd,
        ymin = lwr,
        ymax = upr,
        alpha = 0.9
      ),
      data = dplyr::filter(bfast_top_tb, after_break == TRUE),
      fill = "gray"
    ) +
    ggplot2::scale_x_continuous(breaks = break_lines) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none"
    )
  return(p)
}



#' Helper function for getting a base map
#'
#' @description
#' Helper function for getting a base map given the plot data.
#'
#' @param plot_data an sf object.
#' @param var name of a variable in plot_data.
#' @param leg a logical(1). Should the legend be included in the plot?
#'
#' @return a ggplot2 object.
#'
#' @export
#'
get_base_map <- function(plot_data, var, leg) {
  p <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = tidyr::drop_na(plot_data),
      mapping = ggplot2::aes(fill = {{ var }}),
      show.legend = leg
    )
  return(p)
}



#' Helper function. Get a base plot of the frequency of categories of p-values
#'
#' @description
#' Helper function for creating a histogram of the frequency of the categories
#' of p-values in the grid.
#'
#' @param plot_data a tibble. Data for the plot.
#' colors.
#'
#' @return a ggplto2 object.
#'
#' @export
#'
get_base_hist <- function(plot_data) {
  pcolor <- pinterval <- rsqr_adj <- NULL
  p <-
    plot_data |>
    sf::st_drop_geometry() |>
    tidyr::drop_na(rsqr_adj) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        y = pinterval,
        fill = pcolor
      )
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      x = "Frequency (number of cells)",
      y = "p-value"
    )
  return(p)
}



#' Get a plot of the given variable
#'
#' @description
#' Given an sf object, build a ggplot2 object of the given variable.
#'
#' @param data_sf an `sf`` object.
#' @param var a variable in `data_sf`.
#' @param polygons_sf an `sf`` object which outline will be plot on top of
#' `data_sf`.
#' @param leg a logical(1). Should the legend be included in the plot?
#'
#' @return a `ggplot2` object.
#'
#' @export
#'
#'
get_plot_sf <- function(data_sf, var, polygons_sf, leg) {
  get_base_map(
    plot_data = data_sf,
    var = {{var}},
    leg = leg
  ) +
  ggplot2::geom_sf(
    data = polygons_sf,
    color = "black",
    lwd = 0.6,
    fill = NA,
    show.legend = FALSE
  )
}


#' Get a plot of the rsquare
#'
#' @description
#' Util function. Get the monthly r-squared plots of the montly linear models.
#'
#' @param lm01_sp a tibble with 2 columns: the on with the month and the
#' list-column with the plot data (`sf` objects).
#' @param var a variable in the column `plot_data` of `lm01_sp`.
#' @param polygons_sf an `sf`` object which outline will be plot on top of
#' `data_sf`.
#' @param leg a logical(1). Should the legend be included in the plot?
#'
#' @export
#'
get_plot_lm01_rsqr <- function(lm01_sp, var, polygons_sf, leg) {
  plot_data <- NULL
  res <-
    lm01_sp |>
    dplyr::mutate(
      plot_lm01_rsqr = purrr::map(
        .x = plot_data,
        .f = get_plot_sf,
        # var = lm01_rsqr_adj,
        var = {{var}},
        polygons_sf = polygons_sf,
        leg = leg
      )
    )
  return(res)
}


#' Get a plot of p.values in a grid
#'
#' @description
#' Util function. Get a `ggplot2` object made of the spatial data in `dat_sf`
#' using its column `var_pvalue` after being categorized usign the data in
#' `breaks_tb`
#'
#' @param data_sf an `sf` object.
#' @param breaks_tb a tibble with the categories for the p.values.
#' @param var_pvalue a columnm in `data_sf` with the p.values.
#' @param polygons_sf an `sf`` object which outline will be plot on top of
#' `data_sf`.
#' @param leg a logical(1). Should the legend be included in the plot?
#'
#' @return a `ggplot2` object.
#'
#' @export
#'
get_plot_lm_pvalues_grid <- function(data_sf, breaks_tb, var_pvalue,
                                     polygons_sf, leg) {
  pcolor <- NULL
  path_row <- pinterval <- NULL
  p <-
    data_sf |>
    classify_pvalues(
      breaks_tb = breaks_tb,
      pvalue_col = {{var_pvalue}}
    ) |>
    dplyr::select(path_row, pinterval, pcolor) |>
    get_plot_sf(
      #NOTE: Use pcolor and don't show the legend. This figure is meant to be
        #accompanied by the histogram. The colors here would match those of the
        #histograms and the readers would understant the color meanings in the
        #map from the histogram.
      var = pcolor,
      polygons_sf = polygons_sf,
      leg = leg
    )
  return(p)
}



#' Get a histogram of p.values
#'
#' @description
#' Util function. Get a `ggplot2` object using the column `var_pvalue` after
#' being categorized usign the data in `breaks_tb`.
#'
#' @param data_sf an `sf` object.
#' @param breaks_tb a tibble with the categories for the p.values.
#' @param var_pvalue a columnm in `data_sf` with the p.values.
#'get_plot_lm01_pvalues_hist
#' @return a `ggplot2` object.
#'
#' @export
#'
get_plot_lm_pvalues_hist <- function(data_sf, breaks_tb, var_pvalue) {
  pcolor <- pinterval <- NULL
  p <-
    data_sf |>
    sf::st_drop_geometry() |>
    classify_pvalues(
      breaks_tb = breaks_tb,
      pvalue_col = {{var_pvalue}}
    ) |>
    dplyr::select(pinterval, pcolor) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        y = pinterval,
        group = pinterval,
        fill = pcolor
      ),
      show.legend = FALSE
    ) +
    ggplot2::labs(
      x = "Number of cells",
      y = "p-value"
    )
  return(p)
}


#' Get a plot of p.values for the montly models' parameters
#'
#' @description
#' Get a `ggplot2` object for each `sp` object contained in it.
#'
#' @param lm_tb a tibble with a column (`plot_data`) of `sf` objects.
#' @param breaks_tb a tibble with the categories for the p.values.
#' @param var_pvalue_intercept,var_pvalue_slope a columnm in `data_sf` with
#' the p.values for the intercept and the slope.
#' @param polygons_sf an `sf`` object which outline will be plot on top of
#' @param leg a logical(1). Should the legend be included in the plot?
#'
#' @return a tibble (`lm_tb`) with additional columns.
#'
#' @export
#'
get_plot_lm01_pvalues_grid <- function(lm_tb, breaks_tb, var_pvalue_intercept,
                                       var_pvalue_slope, polygons_sf, leg){
  plot_data <- NULL
  p <-
    lm_tb |>
    dplyr::mutate(
      plot_lm01_pvalue_intercept = purrr::map(
        .x = plot_data,
        .f = get_plot_lm_pvalues_grid,
        breaks_tb = breaks_tb,
        #var_pvalue = lm01_lm01_pvalue_intercept,
        var_pvalue = {{var_pvalue_intercept}},
        polygons_sf = polygons_sf,
        leg = leg
      ),
      plot_lm01_pvalue_slope = purrr::map(
        .x = plot_data,
        .f = get_plot_lm_pvalues_grid,
        breaks_tb = breaks_tb,
        #var_pvalue = lm01_lm01_pvalue_slope,
        var_pvalue = {{var_pvalue_slope}},
        polygons_sf = polygons_sf,
        leg = leg
      )
    )
  return(p)
}



#' Get a histogram of p.values of the lm01 models
#'
#' @description
#' Util function. Get a `ggplot2` object using the column `var_pvalue` after
#' being categorized usign the data in `breaks_tb`.
#'
#' @param lm_tb a tibble with a column (`plot_data`) of `sf` objects.
#' @param breaks_tb a tibble with the categories for the p.values.
#' @param var_pvalue_intercept,var_pvalue_slope a columnm in `data_sf` with
#' the p.values for the intercept and the slope.
#'
#' @return a `ggplot2` object.
#'
#' @export
#'
get_plot_lm01_pvalues_hist <- function(lm_tb, breaks_tb, var_pvalue_intercept,
                                       var_pvalue_slope){
  plot_data <- NULL
  p <-
    lm_tb |>
    dplyr::mutate(
      plot_lm01_pvalue_intercept_hist = purrr::map(
        .x = plot_data,
        .f = get_plot_lm_pvalues_hist,
        breaks_tb = breaks_tb,
        #var_pvalue = lm01_lm01_pvalue_intercept #TODO: remove this line!
        var_pvalue = {{var_pvalue_intercept}}
      ),
      plot_lm01_pvalue_slope_hist = purrr::map(
        .x = plot_data,
        .f = get_plot_lm_pvalues_hist,
        breaks_tb = breaks_tb,
        #var_pvalue = lm01_lm01_pvalue_slope #TODO: remove this line!
        var_pvalue = {{var_pvalue_slope}}
      )
    )
  return(p)
}
