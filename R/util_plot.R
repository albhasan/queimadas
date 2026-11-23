#' Get data for the monthly plots
#'
#' @description
#' Helper function. Build monthly sf objects given a grid and a data frame of
#' yearly data.
#'
#' @param lm01_tb a tibble.
#' @param grid_sf an sf object representing a grid.
#'
#' @return a tibble.
#'
#' @export
#'
sp_join_month <- function(lm01_tb, grid_sf) {
  dat_month <- lm01_month <- plot_data <- NULL
  plot_tb <-
    lm01_tb |>
      # Nest data by month.
      tidyr::nest(
        .by = tidyselect::all_of("lm01_month"),
        .key = "dat_month"
      ) |>
      # Join monthly data to the grid.
      dplyr::mutate(
        plot_data = purrr::map(
          .x = dat_month,
          .f = function(dat_month, grid_sf) {
            return(
              grid_sf |>
                dplyr::left_join(
                  y = dat_month,
                  by = "path_row",
                  keep = FALSE
                )
            )
          },
          grid_sf = grid_sf
        )
      ) |>
      dplyr::select(lm01_month, plot_data)
  return(plot_tb)
}

#' Write a plot to disc
#'
#' @description
#' Utilitary function. Save the given `ggplot2` object to the given file.
#'
#' @param aplot a `ggplto2` object.
#' @param out_file a character(1). A file path.             
#' @param plot_size a list.
#'
#' @return a character: `out_file`.
#'
write_plot <- function(aplot, out_file, plot_size) {
  ggplot2::ggsave(
    filename = out_file,
    plot = aplot,
    width = plot_size[["width"]],
    height = plot_size[["height"]],
    units = plot_size[["units"]]
  )
  return(out_file)
}



#' Build a file name for a plot
#'
#' @description
#' Utilitary function. Join the given attributes into a file name for plot.
#'
#' @param out_dir a character(1). Path to a directory.
#' @param sat_x,sat_y a character(1). Name of a satellite.
#' @param sat_sep a character(1). A separator to use between satellite names.
#' @param suffix a characeter(1). Append this to the name, including its file
#' extension.
#'
#' @return a character(1).
#'
get_plot_fname <- function(out_dir, sat_x, sat_sep, sat_y, suffix) {
  out_file <-
    file.path(
      out_dir,
      paste(sat_x, "xy", sat_y, suffix, sep = "_")
    )
  return(out_file)
}



#' Save plot to file
#'
#' @description
#' Utilitary function. Write the given plot to disc.
#'
#' @param plot_lm12_rsqr A plot of the r-squared value of the linear model 
#' adjustd using 12-month samples, independent of the year.
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_size a list made of the width, height, and units (e.g. cms) of
#' the desired plot. 
#' @param out_dir a character(1). Path to a directory.
#'
#' @return a character(1). A path to a PNG file.
#'
#' @export
#'
save_plot_lm12_rsqr <- function(plot_lm12_rsqr, satelite_x, satelite_y,
                                plot_size, out_dir) {
  out_file <-
    get_plot_fname(
      out_dir = out_dir,
      sat_x = satelite_x,
      sat_sep <- "xy",
      sat_y = satelite_y,
      suffix = "plot_lm12_rsqr.png"
    )
  new_file <-
    write_plot(
    aplot = plot_lm12_rsqr,
    out_file = out_file,
    plot_size = plot_size
  )
  return(new_file)
}



#' Save grid plot to file
#'
#' @description
#' Utilitary function. Write the given plot to disc.
#'
#' @param plot_lm12_pvalue_intercept_grid a plot of the r-squared value of the
#' linear model adjustd using 12-month samples, independent of the year.
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_size a list made of the width, height, and units (e.g. cms) of
#' the desired plot. 
#' @param out_dir a character(1). Path to a directory.
#'
#' @return a character(1). A path to a PNG file.
#'
#' @export
#'
save_plot_lm12_pvalue_intercept_grid <- function(
  plot_lm12_pvalue_intercept_grid, satelite_x, satelite_y, plot_size, out_dir
) {
  out_file <-
    get_plot_fname(
      out_dir = out_dir,
      sat_x = satelite_x,
      sat_sep <- "xy",
      sat_y = satelite_y,
      suffix = "plot_lm12_pvalue_intercept_grid.png"
    )
  new_file <-
    write_plot(
    aplot = plot_lm12_pvalue_intercept_grid,
    out_file = out_file,
    plot_size = plot_size
  )
  return(new_file)
}



#' Save histogram to file
#'
#' @description
#' Utilitary function. Write the given plot to disc.
#'
#' @param plot_lm12_pvalue_intercept_hist a plot of the r-squared value of the
#' linear model adjustd using 12-month samples, independent of the year.
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_size a list made of the width, height, and units (e.g. cms) of
#' the desired plot. 
#' @param out_dir a character(1). Path to a directory.
#'
#' @return a character(1). A path to a PNG file.
#'
#' @export
#'
save_plot_lm12_pvalue_intercept_hist <- function(
  plot_lm12_pvalue_intercept_hist, satelite_x, satelite_y, plot_size, out_dir
) {
  out_file <-
    get_plot_fname(
      out_dir = out_dir,
      sat_x = satelite_x,
      sat_sep <- "xy",
      sat_y = satelite_y,
      suffix = "plot_lm12_pvalue_intercept_hist.png"
    )
  new_file <-
    write_plot(
    aplot = plot_lm12_pvalue_intercept_hist,
    out_file = out_file,
    plot_size = plot_size
  )
  return(new_file)
}






#' Save grid plot to file
#'
#' @description
#' Utilitary function. Write the given plot to disc.
#'
#' @param plot_lm12_pvalue_slope_grid a plot of the r-squared value of the
#' linear model adjustd using 12-month samples, independent of the year.
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_size a list made of the width, height, and units (e.g. cms) of
#' the desired plot. 
#' @param out_dir a character(1). Path to a directory.
#'
#' @return a character(1). A path to a PNG file.
#'
#' @export
#'
save_plot_lm12_pvalue_slope_grid <- function(plot_lm12_pvalue_slope_grid,
                                             satelite_x, satelite_y,
                                             plot_size, out_dir) {
  out_file <-
    get_plot_fname(
      out_dir = out_dir,
      sat_x = satelite_x,
      sat_sep <- "xy",
      sat_y = satelite_y,
      suffix = "plot_lm12_pvalue_slope_grid.png"
    )
  new_file <-
    write_plot(
    aplot = plot_lm12_pvalue_slope_grid,
    out_file = out_file,
    plot_size = plot_size
  )
  return(new_file)
}



#' Save histogram to file
#'
#' @description
#' Utilitary function. Write the given histogram to disc.
#'
#' @param plot_lm12_pvalue_slope_hist a plot of the r-squared value of the
#' linear model adjustd using 12-month samples, independent of the year.
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_size a list made of the width, height, and units (e.g. cms) of
#' the desired plot. 
#' @param out_dir a character(1). Path to a directory.
#'
#' @return a character(1). A path to a PNG file.
#'
#' @export
#'
save_plot_lm12_pvalue_slope_hist <- function(plot_lm12_pvalue_slope_hist,
                                             satelite_x, satelite_y,
                                             plot_size, out_dir) {
  out_file <-
    get_plot_fname(
      out_dir = out_dir,
      sat_x = satelite_x,
      sat_sep <- "xy",
      sat_y = satelite_y,
      suffix = "plot_lm12_pvalue_slope_hist.png"
    )
  new_file <-
    write_plot(
    aplot = plot_lm12_pvalue_slope_hist,
    out_file = out_file,
    plot_size = plot_size
  )
  return(new_file)
}



#' Write the plot of the rsqaure of a linear model
#'
#' @description
#' Utilitary function meant to be call from `save_plot_lm01_rsqr`.
#'
#' @param plot_lm01_rsqr_tb a tibble with two columns, one for the month and
#' another with `ggplot2` objects.
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_size a list made of the width, height, and units (e.g. cms) of
#' the desired plot. 
#' @param out_dir a character(1). Path to a directory.
#'
#' @return a tibble, `plot_lm01_rsqr_tb` with an additional column
#' (`path_plot_lm01_rsqr`).
#'
#' @export
#'
save_plot_lm01_rsqr <- function(plot_lm01_rsqr_tb, satelite_x, satelite_y,
                                plot_size, out_dir) {
  stopifnot("Tibble expected!" = inherits(x = plot_lm01_rsqr_tb, what = "tbl"))
  lm01_month <- plot_lm01_rsqr <- NULL
  res <- 
    plot_lm01_rsqr_tb |>
    dplyr::mutate(
      path_plot_lm01_rsqr = purrr::pmap_chr(
        .l = list(plot_lm01_rsqr, lm01_month),
        .f = save_plot_lm01_rsqr_util,
        satelite_x = satelite_x,
        satelite_y = satelite_y,
        plot_size = plot_size,
        out_dir = out_dir
      )
    )
  return(res)
}



#' Write the plot of the rsquared of a linear model
#'
#' @description
#' Utilitary function meant to be call from `save_plot_lm01_rsqr`.
#'
#' @param plot_lm01_rsqr a `ggplot2` object. Plot of the r-squared of a monthly
#' regression.
#' @param lm01_month a character(1). Month identifier.
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_size a list made of the width, height, and units (e.g. cms) of
#' the desired plot. 
#' @param out_dir a character(1). Path to a directory.
#'
#' @return a character(1). Path to a PNG file.
#'
save_plot_lm01_rsqr_util <- function(plot_lm01_rsqr, lm01_month,
                                     satelite_x, satelite_y,
                                     plot_size, out_dir) {
  out_file <-
    file.path(
      out_dir,
      paste0(
        paste(satelite_x, "xy", satelite_y,
            "plot_lm01_rsqr", lm01_month, sep = "_"),
        ".png"
      )
    )
  stopifnot("Tibble expected!" = inherits(x = plot_lm01_rsqr, what = "ggplot"))
  new_file <-
    write_plot(
    aplot = plot_lm01_rsqr,
    out_file = out_file,
    plot_size = plot_size
  )
  return(new_file)
}



#' Save grid plot to file
#'
#' @description
#' Utilitary function. Write the given plot to disc.
#'
#' @param plot_lm01_pvalue_grid_tb a tibble with the plots of the p.values
#' (intercept and slope) for each month.
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_size a list made of the width, height, and units (e.g. cms) of
#' the desired plot. 
#' @param out_dir a character(1). Path to a directory.
#'
#' @return a character(1). A path to a PNG file.
#'
#' @export
#'
save_plot_lm01_pvalue_intercept_grid <- function(plot_lm01_pvalue_grid_tb,
                                                 satelite_x, satelite_y,
                                                 plot_size, out_dir) {
  lm01_month <- plot_lm01_pvalue_intercept <- NULL
  plot_lm01_pvalue_intercept_grid <- NULL
  res <-
    plot_lm01_pvalue_grid_tb |>
    dplyr::mutate(
      path_plot_lm01_pvalue_intercept_grid = purrr::pmap_chr(
        #TODO: Update this column name to indicate it is a grid plot.
        #.l = list(plot_lm01_pvalue_intercept_grid, lm01_month),
        .l = list(plot_lm01_pvalue_intercept, lm01_month),
        .f = save_plot_lm01_pvalue_intercept_grid_util,
        satelite_x = satelite_x,
        satelite_y = satelite_y,
        plot_size = plot_size,
        out_dir = out_dir
      )
    )
  return(res)
}



#' Save grid plot to file
#'
#' @description
#' Utilitary function. Write the given plot to disc.
#'
#' @param plot_lm01_pvalue_intercept_grid A plot of the pvalue value of the
#' intercept  of the 12-month adjusted linear model.
#' @param lm01_month a character(1). Month of the linear model in the plot.
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_size a list made of the width, height, and units (e.g. cms) of
#' the desired plot. 
#' @param out_dir a character(1). Path to a directory.
#'
#' @return a character(1). A path to a PNG file.
#'
save_plot_lm01_pvalue_intercept_grid_util <- function(
  plot_lm01_pvalue_intercept_grid, lm01_month, satelite_x, satelite_y,
  plot_size, out_dir
) {
  out_file <-
    get_plot_fname(
      out_dir = out_dir,
      sat_x = satelite_x,
      sat_sep <- "xy",
      sat_y = satelite_y,
      suffix = paste("plot_lm01", lm01_month, "pvalue_intercept_grid.png",
        sep = "_")
    )
  new_file <-
    write_plot(
    aplot = plot_lm01_pvalue_intercept_grid,
    out_file = out_file,
    plot_size = plot_size
  )
  return(new_file)
}



#' Save plot to file
#'
#' @description
#' Utilitary function. Write the given plot to disc.
#'
#' @param plot_lm01_pvalue_grid_tb a tibble with the plots of the p.values
#' (intercept and slope) for each month.
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_size a list made of the width, height, and units (e.g. cms) of
#' the desired plot. 
#' @param out_dir a character(1). Path to a directory.
#'
#' @return a character(1). A path to a PNG file.
#'
#' @export
#'
save_plot_lm01_pvalue_slope_grid <- function(plot_lm01_pvalue_grid_tb,
                                             satelite_x, satelite_y,
                                             plot_size, out_dir) {
  lm01_month <- NULL
  plot_lm01_pvalue_slope <- NULL
  #plot_lm01_pvalue_slope_grid <- NULL
  res <-
    plot_lm01_pvalue_grid_tb |>
    dplyr::mutate(
      path_plot_lm01_pvalue_slope = purrr::pmap_chr(
        #TODO: Update this column name to indicate it is a grid plot.
        #.l = list(plot_lm01_pvalue_slope_grid, lm01_month),
        .l = list(plot_lm01_pvalue_slope, lm01_month),
        .f = save_plot_lm01_pvalue_slope_grid_util,
        satelite_x = satelite_x,
        satelite_y = satelite_y,
        plot_size = plot_size,
        out_dir = out_dir
      )
    )
  return(res)
}



#' Save plot to file
#'
#' @description
#' Utilitary function. Write the given plot to disc.
#'
#' @param plot_lm01_pvalue_slope A plot of the pvalue value of the
#' slope  of the 12-month adjusted linear model.
#' @param lm01_month a character(1). Month of the linear model in the plot.
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_size a list made of the width, height, and units (e.g. cms) of
#' the desired plot. 
#' @param out_dir a character(1). Path to a directory.
#'
#' @return a character(1). A path to a PNG file.
#'
save_plot_lm01_pvalue_slope_grid_util <- function(plot_lm01_pvalue_slope,
                                                  lm01_month,
                                                  satelite_x, satelite_y,
                                                  plot_size, out_dir) {
  out_file <-
    get_plot_fname(
      out_dir = out_dir,
      sat_x = satelite_x,
      sat_sep <- "xy",
      sat_y = satelite_y,
      suffix = paste("plot_lm01", lm01_month, "pvalue_slope.png",
        sep = "_")
    )
  new_file <-
    write_plot(
    aplot = plot_lm01_pvalue_slope,
    out_file = out_file,
    plot_size = plot_size
  )
  return(new_file)
}



#' Prepare data for forecast plot
#'
#' @description
#' Utilitary function. Given tibbles for the observations and their forecast,
#' prepare a table on which the original observations are preserved and the
#' empty ones are filled with forecasted values.
#'
#' @param x_df,y_df a data frame with observations.
#' @param forecast_df a data frame with the results of fitting a linear
#' model `y ~ x` and use it to forecast the x values.
#'
#' @return a tibble with the x & y observations with additional columns
#' (prefixed with `ts`).
#'
#' @export
#'
build_plot_data_ts <- function(x_df, y_df, forecast_df) {
  period <- n.x <- n.y <- fit <- lwr <- upr <- y <- NULL
  stopifnot("Data mismatch!" = nrow(x_df) == nrow(forecast_df))
  plot_data_tb <-
    tibble::tibble(
      period  = sort(unique(c(x_df[["period"]], y_df[["period"]])))
    ) |>
    dplyr::left_join(
      y = dplyr::bind_cols(x_df, forecast_df),
      by = dplyr::join_by(period)
    ) |>
    dplyr::left_join(
      y = y_df ,
      by = dplyr::join_by(period)
    ) |>
    dplyr::select(period, x = n.x, y = n.y, fit, lwr, upr) |> 
    dplyr::mutate(
      ts_value = dplyr::if_else(
        condition = is.na(y),
        # Avoid negative predictions.
        true = pmax(0, fit, na.rm = TRUE),
        false = y
      )
    ) |>
    dplyr::mutate(
      ts_type = dplyr::if_else(
        condition = is.na(y),
        true = "Forecasted",
        false = "Observed"
      )
    ) |>
    dplyr::mutate(
      ts_lwr = dplyr::if_else(
        condition = is.na(y),
        # Avoid negative confidence intervals.
        # NOTE: pmax is the vectorized version of max.
        true = pmax(0, lwr, na.rm = TRUE),
        false = y
      )
    ) |>
    dplyr::mutate(
      ts_upr = dplyr::if_else(
        condition = is.na(y),
        # Avoid negative confidence intervals.
        # NOTE: pmax is the vectorized version of max.
        true = pmax(0, upr, na.rm = TRUE),
        false = y
      )
    )
  return(plot_data_tb)
}



#' Get a time series plot
#'
#' @description
#' Use the given data to produce a plot of both observations
#' and forecasted values, including confidence intervals.
#'
#' @param plot_data a tibble.
#'
#' @return a `ggplot2` object.
#'
#' @export
#'
get_plot_forecast_ts <- function(plot_data) {
  period <- period_date <- ts_lwr <- ts_upr <- ts_value <- ts_type <- NULL
  if(length(plot_data) == 1 && is.na(plot_data)) {
    return(NA)
  }
  p <-
    plot_data |>
    dplyr::mutate(
      period_date = period_to_date(period)
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        x = period_date,
        ymin = ts_lwr,
        ymax = ts_upr,
      ),
      fill = "gray80"
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = period_date,
        y = ts_value
      )
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = period_date,
        y = ts_value,
        color = ts_type,
        shape = ts_type
      ),
      size = 2
    ) +
    ggplot2::labs(
      x = "Time",
      y = "Active fires"
    )
  return(p)
}



#' Prepare data for forecast plot
#'
#' @description
#' Utilitary function. Given tibbles for the observations and their forecast,
#' prepare a table on which the original observations are preserved and the
#' empty ones are filled with forecasted values.
#'
#' @param x_df,y_df a data frame with observations.
#' @param forecast_01_df a data frame with the results of fitting a linear
#' model `y ~ x` and use it to forecast the x values.
#'
#' @return a tibble with the x & y observations with additional columns
#' (prefixed with `ts`), or NA.
#'
#' @export
#'
build_lm01_plot_data_ts <- function(x_df, y_df, forecast_01_df) {
  fit <- lwr <- upr <- NULL
  if(length(forecast_01_df) == 1 && is.na(forecast_01_df)) {
    return(NA)
  }
  return(
    build_plot_data_ts(
      x_df = x_df,
      y_df = y_df,
      #TODO: Make the columns in forecast_01_df & forecast_12_df match!
      #      Go where they were created and ensure they have the same columns.
      #      Then come back here, remove this function, and refactor.
      forecast_df = dplyr::select(forecast_01_df, fit, lwr, upr)
    )
  )
}



#' Get a time series plot
#'
#' @description
#' Build time series data and plots for given the data in the input tibble.
#'
#' @param forecast_tb  a tibble.
#'
#' @return a tibble;`forecast_tb` with additional columns.
#'
#' @export
#'
get_plot_forecast <- function(forecast_tb) {
  forecast_12_df <- plot_data <- x_df <- y_df <- NULL
  plot_lm12_data <- forecast_01_df <- plot_lm01_data <- NULL
  res <-
    forecast_tb |>
    dplyr::mutate(
      plot_lm12_data = purrr::pmap(
        .l = list(x_df, y_df, forecast_12_df),
        .f = build_plot_data_ts
      ),
       plot_lm12_ts = purrr::map(
        .x = plot_lm12_data,
        .f = get_plot_forecast_ts
      ),
      plot_lm01_data = purrr::pmap(
        .l = list(x_df, y_df, forecast_01_df),
        .f = build_lm01_plot_data_ts
      ),
      plot_lm01_ts = purrr::map(
       .x = plot_lm01_data,
       .f = get_plot_forecast_ts
      )
    )
  return(res)
}



#' Save the forecast data
#'
#' @description
#' Write to disc the forecast data used created the plots.
#'
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_tb a tibble with the forecast data and plots. 
#' @param out_dir_tables a character(1). Path to a directory.
#'
#' @return a tibble with path to the created files (2 columns, one for each
#' forecast from the yearly and monthly models).
#'
#' @export
#'
save_forecast_data <- function(satelite_x, satelite_y, plot_tb,
                               out_dir_tables) {
  path_row <- overlap_ts <- plot_lm12_data <- plot_lm01_data <- has_data <- NULL

  lm12_ts <-
    plot_tb |>
    dplyr::select(path_row, overlap_ts, plot_lm12_data) |>
    tidyr::unnest(plot_lm12_data)

  lm01_ts <-
    plot_tb |>
    dplyr::select(path_row, overlap_ts, plot_lm01_data) |>
    dplyr::mutate(
      has_data = purrr::map_lgl(
        .x = plot_lm01_data,
        .f = has_df_data
      )
    ) |>
    dplyr::filter(has_data == TRUE) |>
    dplyr::select(-has_data) |>
    tidyr::unnest(plot_lm01_data)

  # Write to disc as CSVs lm12_ts and lm01_ts
  forecast_lm12_ts_file <- get_plot_fname(
    out_dir = out_dir_tables,
    sat_x = satelite_x,
    sat_sep = "_xy_",
    sat_y = satelite_y,
    suffix = "forecast_lm12_ts.csv"
  )
  readr::write_csv(
    x = lm12_ts,
    file = forecast_lm12_ts_file
  )
  forecast_lm01_ts_file <- get_plot_fname(
    out_dir = out_dir_tables,
    sat_x = satelite_x,
    sat_sep = "_xy_",
    sat_y = satelite_y,
    suffix = "forecast_lm01_ts.csv"
  )
  readr::write_csv(
    x = lm01_ts,
    file = forecast_lm01_ts_file
  )

  return(
    tibble::tibble(
      forecast_lm12_ts_file = forecast_lm12_ts_file,
      forecast_lm01_ts_file = forecast_lm01_ts_file 
    )
  )
}



#' Save the forecast data
#'
#' @description
#' Write to disc the forecast data used created the plots.
#'
#' @param satelite_x,satelite_y a character(1). Names of the observation
#' satelites used for the regression `y ~ x`.
#' @param plot_tb a tibble with the forecast data and plots. 
#' @param out_dir_figures a character(1). Path to a directory.
#'
#' @return a data frame with the path row, the plot, and the file path to the
#' plot file for both lm12 and lm01 plots.
#'
#' @export
#'
save_forecast_plot <- function(satelite_x, satelite_y, plot_tb,
                               out_dir_figures) {
  path_row <- plot_lm12_ts <- plot_lm01_ts <- has_plot <- plot_lm12_ts <- NULL

  lm12_plot <-
    plot_tb |>
    dplyr::select(path_row, plot_lm12_ts) |>
    dplyr::mutate(
      file_plot_lm12_ts = purrr::pmap_chr(
        .l = list(path_row, plot_lm12_ts, satelite_x, satelite_y,
            out_dir_figures),
        .f = function(path_row, plot_lm12_ts, satelite_x, satelite_y,
                      out_dir_figures) {
          forecast_lm12_plot_file <- get_plot_fname(
            out_dir = out_dir_figures,
            sat_x = satelite_x,
            sat_sep = "_xy_",
            sat_y = satelite_y,
            suffix = paste0("forecast_lm12_", path_row, "_plot.png")
          )
          ggplot2::ggsave(
            filename = forecast_lm12_plot_file,
            plot = plot_lm12_ts
          )
          return(forecast_lm12_plot_file)
        }
      )
    )

  lm01_plot <-
    plot_tb |>
    dplyr::select(path_row, plot_lm01_ts) |>
    dplyr::mutate(
      has_plot = purrr::map_lgl(
        .x = plot_lm01_ts,
        .f = inherits,
        what = "ggplot2"
      )
    ) |>
    dplyr::filter(has_plot == TRUE) |>
    dplyr::select(-has_plot) |>
    dplyr::mutate(
      file_plot_lm01_ts = purrr::pmap_chr(
        .l = list(path_row, plot_lm01_ts, satelite_x, satelite_y,
            out_dir_figures),
        .f = function(path_row, plot_lm01_ts, satelite_x, satelite_y,
                      out_dir_figures) {
          forecast_lm12_plot_file <- get_plot_fname(
            out_dir = out_dir_figures,
            sat_x = satelite_x,
            sat_sep = "_xy_",
            sat_y = satelite_y,
            suffix = paste0("forecast_lm12_", path_row, "_plot.png")
          )
          ggplot2::ggsave(
            filename = forecast_lm12_plot_file,
            plot = plot_lm12_ts
          )
          return(forecast_lm12_plot_file)
        }
      )
    )

  res <-
    dplyr::left_join(
      x = lm12_plot,
      y = lm01_plot,
      by = dplyr::join_by(path_row)
    )

  return(res)
}
