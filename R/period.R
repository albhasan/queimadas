#' Is the period valid?
#'
#' @description
#' Check if the given period is valid.
#'
#' @param x a character representing a period (e.g. '2007-01').
#'
#' @return a logical(1).
#'
#' @export
#'
is_period_valid <- function(x) {
  if (is.character(x)) {
    if (all(is.na(x) == FALSE)) {
      if (length(x) > 0) {
        if (all(grepl(
          pattern = "^[[:digit:]]{4}-[[:digit:]]{2}$",
          x = x,
          ignore.case = FALSE,
          perl = FALSE,
          fixed = FALSE,
          useBytes = FALSE
        ))) {
          return(TRUE)
        }
      }
    }
  }
  return(FALSE)
}


#' Convert from period to date
#'
#' @description
#' Given a period (e.g. '2007-01'), convert it into a date object.
#'
#' @param x a character.
#'
#' @return a date object (`base::Date`).
#'
#' @export
#'
period_to_date <- function(x) {
  stopifnot("Invalid period!" = is_period_valid(x))
  period_date <- paste(x, "01", sep = "-")
  return(as.Date(period_date))
}


#' Convert a date into a period
#'
#' @description
#' Given a date (base::Date), return its period.
#'
#' @param x a date object.
#'
#' @return a character.
#'
#' @export
#'
date_to_period <- function(x) {
  stopifnot(inherits(x = x, what = "Date"))
  return(substr(
    x = as.character(x),
    start = 1,
    stop = 7
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
  stopifnot("Invalid period!" = is_period_valid(x))
  return(
    substr(
      x = x,
      start = 6L,
      stop = 7L
    )
  )
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
