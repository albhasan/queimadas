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
#' overlaping observations using the observations' month, but ignoring
#' their year.
#'
#' @param x_df a data frame.
#' @param y_df a data frame.
#' @param formula a character. A formula object (see `stats::formula`).
#' @param clevel a numeric(1). The confidence level for estimating the
#' confidence interval.
#'
#' @return a list with two objects: a single `stats::lm` object and a data
#' frame with the data used to fit the model.
#
#' @export
#'
fit_lm_12_months <- function(x_df, y_df, formula = "y ~ x", clevel = 0.95) {
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
  return(
    fit_lm(
      data_df = data_df,
      formula = formula,
      clevel = clevel
    )
  )
}

#' Utilitary function to fit a linear model month by month
#'
#' @description
#' Given two data frames, merge them and then fit a linear model for each
#' month, ignoring the observations' year.
#'
#' @param x_df a data frame.
#' @param y_df a data frame.
#' @param formula a character. A formula object (see `stats::formula`).
#' @param clevel a numeric(1). The confidence level for estimating the
#' confidence interval.
#'
#' @return a list of 12 where each element is composed of a `stats::lm` object
#' and a data frame with the data used to fit the model.
#
#' @export
#'
fit_lm_01_months <- function(x_df, y_df, formula = "y ~ x", clevel = 0.95) {
  cname <- "period"
  colnames(x_df)[2] <- "x"
  colnames(y_df)[2] <- "y"
  data_df <- get_ts_overlap(x_df = x_df, y_df = y_df, cname = cname)
  if (nrow(data_df) == 0) {
    return(list(
      model_01 = NA,
      model_02 = NA,
      model_03 = NA,
      model_04 = NA,
      model_05 = NA,
      model_06 = NA,
      model_07 = NA,
      model_08 = NA,
      model_09 = NA,
      model_10 = NA,
      model_11 = NA,
      model_12 = NA
    ))
  }
  data_df["m"] <- get_month_from_period(x = data_df[[cname]])
  data_df["year"] <- get_year_from_period(x = data_df[[cname]])
  data_df <- data_df[c("year", "m", "x", "y")]

  data_ls <- split(
    x = data_df,
    f = data_df[["m"]],
    drop = FALSE
  )
  names(data_ls) <- paste0("month_", names(data_ls))

  return(
    lapply(
      X = data_ls,
      FUN = fit_lm,
      formula = formula,
      clevel = clevel
    )
  )
}

#' fit a linear model
#'
#' @description
#' Fit a linear model to the given data.
#'
#' @param data_df a data frame.
#' @param formula a character. A formula object (see `stats::formula`).
#' @param clevel a numeric(1). The confidence level for estimating the
#' confidence interval.
#'
#' @return a list with two objects: an `stats::lm` object and a the input data
#' frame with addicional columns.
#' the data used to fit the model.
#
fit_lm <- function(data_df, formula, clevel) {
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


#' Predict month by month including a confidence level
#'
#' @description
#' Use the given models to predict, including the lower and upper values at the
#' given confidence level.
#'
#' @param lm_ls a list of models (`stats::lm`).
#' @param new_data a data frame with data to predict using the given models.
#' @param clevel a numeric(1). The confidence level to use.
#'
#' @return a list of data frames.
#'
#' @export
#'
predict_ci_01 <- function(lm_ls, new_data, clevel) {
  new_data["m"] <- substr(x = new_data[["period"]], start = 6, stop = 7)
  data_ls <- split(x = new_data, f = new_data[["m"]])
  names(data_ls) <- paste0("month_", names(data_ls))

  stopifnot(
    "The number of models must match the number of models!" =
      length(lm_ls) == length(data_ls)
  )

  pred_ls <-
    lapply(
      X = seq_along(data_ls),
      FUN = function(i, data_ls, lm_ls) {
        data_df <- data_ls[[i]]
        new_data <- data_df[["n"]]
        predict_ci(
          lm_obj = lm_ls[[i]],
          new_data = new_data,
          clevel = clevel
        )
      },
      data_ls = data_ls,
      lm_ls = lm_ls
    )

  pred_ls <-
    lapply(
      X = seq_along(pred_ls),
      FUN = function(i, pred_ls, data_ls) {
        pred_df <- pred_ls[[i]]
        data_df <- data_ls[[i]]
        stopifnot(
          "Incompatible data and prediction data frames!" =
            nrow(pred_df) == nrow(data_df)
        )
        return(cbind(data_df, pred_df))
      },
      pred_ls = pred_ls,
      data_ls = data_ls
    )
  names(pred_ls) <- names(lm_ls)

  return(pred_ls)
}
