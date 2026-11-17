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



#' Save plot to file
#'
#' @description
#' Utilitary function. Write the given plot to disc.
#'
#' @param plot_lm12_pvalue_intercept a plot of the r-squared value of the linear model 
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
save_plot_lm12_pvalue_intercept <- function(plot_lm12_pvalue_intercept,
                                            satelite_x, satelite_y,
                                            plot_size, out_dir) {
  out_file <-
    get_plot_fname(
      out_dir = out_dir,
      sat_x = satelite_x,
      sat_sep <- "xy",
      sat_y = satelite_y,
      suffix = "plot_lm12_pvalue_intercept.png"
    )
  new_file <-
    write_plot(
    aplot = plot_lm12_pvalue_intercept,
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
#' @param plot_lm12_pvalue_slope a plot of the r-squared value of the linear model 
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
save_plot_lm12_pvalue_slope <- function(plot_lm12_pvalue_slope,
                                        satelite_x, satelite_y,
                                        plot_size, out_dir) {
  out_file <-
    get_plot_fname(
      out_dir = out_dir,
      sat_x = satelite_x,
      sat_sep <- "xy",
      sat_y = satelite_y,
      suffix = "plot_lm12_pvalue_slope.png"
    )
  new_file <-
    write_plot(
    aplot = plot_lm12_pvalue_slope,
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



#' Save plot to file
#'
#' @description
#' Utilitary function. Write the given plot to disc.
#'
#' @param plot_lm01_pvalue_tb a tibble with the plots of the p.values
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
save_plot_lm01_pvalue_intercept <- function(plot_lm01_pvalue_tb,
                                            satelite_x, satelite_y,
                                            plot_size, out_dir) {
  lm01_month <- plot_lm01_pvalue_intercept <- NULL
  res <-
    plot_lm01_pvalue_tb |>
    dplyr::mutate(
      path_plot_lm01_pvalue_intercept= purrr::pmap_chr(
        .l = list(plot_lm01_pvalue_intercept, lm01_month),
        .f = save_plot_lm01_pvalue_intercept_util,
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
#' @param plot_lm01_pvalue_intercept A plot of the pvalue value of the
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
save_plot_lm01_pvalue_intercept_util <- function(plot_lm01_pvalue_intercept,
                                                 lm01_month,
                                                 satelite_x, satelite_y,
                                                 plot_size, out_dir) {
  out_file <-
    get_plot_fname(
      out_dir = out_dir,
      sat_x = satelite_x,
      sat_sep <- "xy",
      sat_y = satelite_y,
      suffix = paste("plot_lm01", lm01_month, "pvalue_intercept.png",
        sep = "_")
    )
  new_file <-
    write_plot(
    aplot = plot_lm01_pvalue_intercept,
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
#' @param plot_lm01_pvalue_tb a tibble with the plots of the p.values
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
save_plot_lm01_pvalue_slope <- function(plot_lm01_pvalue_tb,
                                        satelite_x, satelite_y,
                                        plot_size, out_dir) {
  lm01_month <- plot_lm01_pvalue_slope <- NULL
  res <-
    plot_lm01_pvalue_tb |>
    dplyr::mutate(
      path_plot_lm01_pvalue_slope = purrr::pmap_chr(
        .l = list(plot_lm01_pvalue_slope, lm01_month),
        .f = save_plot_lm01_pvalue_slope_util,
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
save_plot_lm01_pvalue_slope_util <- function(plot_lm01_pvalue_slope,
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

