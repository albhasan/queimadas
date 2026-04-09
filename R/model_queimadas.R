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
#' Given two data frames, merge them and fit a linear model using their
#' overlaping observations while and using the observations' month (ignoring
#' their year).
#'
#' @param x_df a data frame.
#' @param y_df a data frame.
#'
#' @return a list with two objects: an `stats::lm` object and a data frame with
#' the data used to fit the model.
#
#' @export
#'
fit_montly_lm <- function(x_df, y_df) {
  colnames(x_df)[2] <- "x"
  colnames(y_df)[2] <- "y"
  data_df <- merge(
    x = x_df,
    y = y_df,
    by = "period"
  )
  data_df["m"] <- get_month_from_period(x = data_df[["period"]])
  data_df <- data_df[c("m", "x", "y")]
  lmodel <-
    stats::lm(
      data = data_df,
      formula = "y ~ x",
    )

  return(list(
    model = lmodel,
    data = data_df
  ))
}

#' Extract the month from the period
#'
#' @description
#' Given a period (e.g. "2028-07"), get the month ("07").
#'
#' @param x a character representing an aggregation period (e.g. "2028-07").
#'
#' @return a chracter. A month.
#'
get_month_from_period <- function(x) {
  stopifnot("Invalid period length!" = length(x) > 0)
  stopifnot(
    "Invalid period!" =
      all(
        grepl(
          pattern = "^[[:digit:]]{4}-[[:digit:]]{2}$",
          x = x,
          ignore.case = FALSE,
          perl = FALSE,
          fixed = FALSE,
          useBytes = FALSE
        )
      )
  )
  return(
    substr(
      x = x,
      start = 6L,
      stop = 7L
    )
  )
}
