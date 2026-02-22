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
  ds <- y <- NULL
  stopifnot(c("ds", "y") %in% colnames(train_tb))
  res <-
    train_tb |>
    dplyr::select(ds, y) |>
    prophet::prophet(
      growth = "linear",
      yearly.seasonality = TRUE,
      weekly.seasonality = FALSE,
      daily.seasonality = FALSE,
      seasonality.mode = "additive"
    )
  return(res)
}
