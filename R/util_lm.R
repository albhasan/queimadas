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

#' Get model's R squared
#'
#' @description
#' Get the R squared coefficient from a regression object.
#'
#' @param lm_obj a linear model object (stats package).
#'
#' @return a numeric.
#'
#' @export
#'
get_lm_r2 <- function(lm_obj) {
  s <- summary(lm_obj)
  return(s[["r.squared"]])
}


#' Get model's residuals
#'
#' @description
#' Get the model's residuals.
#'
#' @param lm_obj a linear model object (stats package).
#'
#' @return a numad numeric.
#'
#' @export
#'
get_lm_residuals <- function(lm_obj) {
  return(lm_obj[["residuals"]])
}

#' Predict including a confidence level
#'
#' @description
#' Use the given model to predict including the lower and upper values at the
#' given confidence level.
#'
#' @param lm_obj a model (`stats::lm`).
#' @param new_data NA or a numeric vector. If a NA is given, then it will use
#' the data in the model for the prediction. Otherwise it will predict for each
#' element in the given vector.
#' @param clevel a numeric(1). The confidence level to use.
#'
#' @return a data frame.
#'
#' @export
#'
predict_ci <- function(lm_obj, new_data = NA, clevel = 0.95) {
  if (length(new_data) == 1 && is.na(new_data)) {
    return(stats::predict(
      object = lm_obj,
      interval = "confidence",
      level = clevel
    ))
  } else {
    stopifnot("Only numeric vectors allowed!" = is.numeric(new_data))
    # Create a new data frame with the same colnames used in the model.
    model_df <- lm_obj[["model"]]
    pred_df <- data.frame(
      y = rep(NA, length = length(new_data)),
      x = new_data
    )
    names(pred_df) <- names(model_df)
    # Predict and return.
    return(stats::predict.lm(
      object = lm_obj,
      newdata = pred_df,
      interval = "confidence",
      level = clevel
    ))
  }
}
