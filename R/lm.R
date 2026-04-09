#' Get the equation from an `lm` object
#'
#' @description
#' Get the equation of a linear model as a character.
#'
#' @param lm_obj an `lm`` object.
#' @param dig an integer. The number of decimal places for the model
#' coefficients.
#'
#' @return a character.
#'
#' @export
#'
get_lm_equation <- function(lm_obj, dig = 3) {
  stopifnot(
    "Linear model object (lm) expected!" =
      inherits(x = lm_obj, what = "lm")
  )
  a <- lm_obj[["coefficients"]]["x"]
  b <- lm_obj[["coefficients"]]["(Intercept)"]
  a <- round(x = a, digits = dig)
  b <- round(x = b, digits = dig)
  if (b < 0) {
    b <- paste0("(", b, ")")
  }
  return(
    sprintf(fmt = "y = %s * x + %s", a, b)
  )
}
