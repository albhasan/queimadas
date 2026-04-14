#' Utilitary function to filter observations by satellite name
#'
#' @description
#' Given a data frame of satellite observations, return those that belong to a
#' specific satellite.
#'
#' @param sat_name a character. Name of a satellite.
#' @param data_df a data frame with satellite data.
#'
#' @return a data frame with the same column as the input data frame, but with
#' the same of fewer rows.
#'
#' @export
#'
get_sat_data <- function(sat_name, data_df) {
  stopifnot(
    "Satellite column not found!" = "satelite" %in% colnames(data_df)
  )
  x_df <- data_df[c(data_df[["satelite"]]) == sat_name, c("period", "n")]

  return(x_df)
}


#' Utilitary function to fit a linear model by month
#'
#' @description
#' Given two data frames, merge them and then fit a linear model using their
#' overlaping observations using the observations' month,  but ignoring
#' their year.
#'
#' @param x_df a data frame.
#' @param y_df a data frame.
#' @param formula a character. A formula object (see `stats::formula`).
#' @param clevel a numeric(1). The confidence level for estimating the
#' confidence interval.
#'
#' @return a list with two objects: an `stats::lm` object and a data frame with
#' the data used to fit the model.
#
#' @export
#'
fit_monthly_lm <- function(x_df, y_df, formula = "y ~ x", clevel = 0.95) {
  cname <- "period"
  colnames(x_df)[2] <- "x"
  colnames(y_df)[2] <- "y"
  data_df <- get_ts_overlap(x_df = x_df, y_df = y_df, cname = cname)
  if (nrow(data_df) == 0) {
    return(list(model = NA, data = NA))
  }
  data_df["m"] <- get_month_from_period(x = data_df[[cname]])
  data_df["year"] <- get_year_from_period(x = data_df[[cname]])
  data_df <- data_df[c("year", "m", "x", "y")]
  lm_obj <-
    stats::lm(
      data = data_df,
      formula = formula,
    )
  pred <- predict_ci(
    lm_obj = lm_obj,
    new_data = NA,
    clevel = clevel
  )
  data_df <- cbind(data_df, pred)
  data_df["residuals"] <- lm_obj[["residuals"]]
  lm_outliers <- find_outliers(get_lm_residuals(lm_obj))
  data_df["is_outlier"] <- FALSE
  data_df[names(lm_outliers), "is_outlier"] <- TRUE

  return(list(
    model = lm_obj,
    data = data_df
  ))
}



#' Extract the year from the period
#'
#' @description
#' Given a period (e.g. "2028-07"), get the year ("2028").
#'
#' @param x a character representing an aggregation period (e.g. "2028-07").
#'
#' @return a character. A year.
#'
get_year_from_period <- function(x) {
  stopifnot("Invalid period!" = is_period_valid(x))
  return(
    substr(
      x = x,
      start = 1L,
      stop = 4L
    )
  )
}



#' Get the overlap length among time series
#'
#' @description
#' Given a pair of time series represented as data frames, return the number
#' of time steps of overlap among them.
#'
#' @param x_df,y_df a data frame.
#' @param cname a character. The name of a common column in both input data
#' frames, which representes the time steps.
#'
#' @return an integer(1). The number of common steps in the time series.
#'
#' @export
#'
overlap_len <- function(x_df, y_df, cname) {
  data_df <- get_ts_overlap(x_df = x_df, y_df = y_df, cname = cname)
  return(nrow(data_df))
}

#' Get overlap among time series
#'
#' @description
#' Get the common time steps among time series
#'
#' @param x_df,y_df a data frame representation of a time series.
#' @param cname a character(1). Name of a column in both input time series.
#'
#' @return a data frame.
#'
get_ts_overlap <- function(x_df, y_df, cname) {
  stopifnot("Column not found in data frame!" = cname %in% colnames(x_df))
  stopifnot("Column not found in data frame!" = cname %in% colnames(y_df))
  data_df <- merge(
    y = y_df,
    x = x_df,
    by = cname
  )
  return(data_df)
}
