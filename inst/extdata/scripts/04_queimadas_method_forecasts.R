# TODO: Implement Fabiano Morelli

#---- Forescat using queimadas approach ----

# NOTE: Not all satelite combinations are valid!
sat_tb <-
  tibble::tribble(
    ~satelite_x, ~satelite_y,
    "AQUA_M-T", "NOAA-12",
    "AQUA_M-T", "NPP-375D",
    "AQUA_M-T", "NPP-375-PM"
  )


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
get_data <-
  function(sat_name, data_df) {
    stopifnot(
      "Satellite column not found!" = "satelite" %in% colnames(data_df)
    )
    x_df <- data_df[c(data_df[["satelite"]]) == sat_name, c("period", "n")]
    return(x_df)
  }

#' Utilitary function to fit a linear model
#'
#' @description
#' Given two data frames, merge then and fit a linear model using the data in
#' them.
#'
#' @param x_df a data frame.
#' @param y_df a data frame.
#'
#' @return a `stats::lm` object.
#'
fit_lm <-
  function(x_df, y_df) {
    colnames(x_df)[2] <- "x"
    colnames(y_df)[2] <- "y"
    data_df <- merge(
      x = x_df,
      y = y_df,
      by = "period"
    )
    data_df["m"] <-
      substr(
        x = data_df[["period"]],
        start = 6L,
        stop = 7L
      )
    data_df <- data_df[c("m", "x", "y")]
    lmodel <-
      lm(
        data = data_df,
        formula = "y ~ x",
      )
    return(lmodel)
  }

sat_tb <-
  sat_tb |>
  dplyr::mutate(
    x_df = purrr::map(
      .x = satelite_x,
      .f = get_data,
      data_df = brazil_ym_tb
    ),
    y_df = purrr::map(
      .x = satelite_y,
      .f = get_data,
      data_df = brazil_ym_tb
    ),
    linear_model = purrr::map2(
      .x = x_df,
      .y = y_df,
      .f = fit_lm
    ),
    forecast = purrr::map2(
      .x = linear_model,
      .y = x_df,
      .f = function(lmodel, data_vec) {
        new_data <- data_vec
        colnames(new_data)[2] <- "x"
        pred_df <-
          data.frame(
            period = data_vec[["period"]],
            n = stats::predict(
              object = lmodel,
              newdata = new_data["x"]
            )
          )
        return(pred_df)
      }
    )
  )

# TODO: plot the lm ojects, compute rediuals, plot them, add them to the slides
#

# TODO: fazer de novo a figura de correlação, usando so o periodo de tempo onde as series de tempo tem sobreposicao.
# TODO: fazer curva regresa por regiao

# no caso do aqua voltar ate 1998
# observaçoes com a metodologia de uma serie temporal
# correlaçao de pearson
# correlacao de pearson da regiao sul
# bfast? perguntar liana sobre o metodo que ela oleo
#
# reumiao semana do 9 a 13
#
# comparaççao do brazil com regiao, como ficaram os residuais,
# \liaja consigue o dia 23

# TODO: Liana olhar segmented regression analysis, modified mamn-kentall, sem slope, loeless regression

#' Plot an `lm` object
plot_lm <- function(lm_obj) {
  ggplog2::ggplot(pred_df, aes(x = x, y = y)) +
    geom_point() +
    geom_line(stat = "function", fun = lm_func, color = "blue")
}
#
