#' Convert to time series
#'
#' @description
#' Utility function to convert the input tibble into a stats::ts object
#' compatible with bfast.
#'
#' @param data_tb a tibble::tibble object.
#' @param n_freq a interger(1). The number of element per cycle.
#'
#' @return a stats::ts object.
#'
#' @export
#'
convert_to_ts <- function(data_tb, n_freq = 12) {
  stopifnot(all(c("ymd", "n") %in% colnames(data_tb)))

  # NOTE: BFast demands data with at least two cycles
  if (nrow(data_tb) < ((2 * n_freq) + 1)) {
    warning("Expected tibble with at least 2 cycles worth of observations.")
    return(NA)
  }

  # NOTE: BFast demands data to be periodic.
  data_tb <- data_tb[1:(nrow(data_tb) %/% n_freq * n_freq), ]

  ts_obj <- stats::ts(
    data = data_tb[["n"]],
    frequency = n_freq,
    start = c(
      lubridate::year(data_tb[[1, "ymd"]]),
      lubridate::month(data_tb[[1, "ymd"]])
    ),
    end = c(
      lubridate::year(data_tb[[nrow(data_tb), "ymd"]]),
      lubridate::month(data_tb[[nrow(data_tb), "ymd"]])
    )
  )
  return(ts_obj)
}

#' Run BFAST
#'
#' @description
#' Utility function to run BFAST.
#'
#' @param x a stats::ts object (see `convert_to_ts`).
#'
#' @return an bfast object.
#'
#' @export
#'
run_bfast <- function(x) {
  if (length(x) == 1 && is.na(x)) {
    return(NA)
  }
  if (length(x) < 24) {
    return(NA)
  }
  bfast::bfast(
    Yt = x,
    season = "harmonic",
    max.iter = 10
  )
}
