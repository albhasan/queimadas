#' Get the pvalues
#'
#' @description
#' Given a tibble with linear model parameters, return the p.values of the
#' intercept and slope.
#'
#' @param m_params a tibble with linear model parameters returned from a call
#' to `broom::tidy`.
#' @param prefix a character(1) for prefixing the new columns names.
#'
#' @return a tibble.
#'
#' @export
#'
get_pvalues_from_lm12_params <- function(m_params, prefix) {
  term <- p.value <- NULL
  res <-
    m_params |>
    dplyr::select(term, p.value) |>
    dplyr::mutate(
      term = dplyr::replace_when(term, term == "(Intercept)" ~ "pvalue_intercept"),
      term = dplyr::replace_when(term, term == "x" ~ "pvalue_slope")
    ) |>
    tidyr::pivot_wider(
      names_from = term,
      values_from = p.value,
      names_prefix = prefix
    )
  return(res)
}



#' Get the pvalues
#'
#' @description
#' Given a tibble with linear model parameters for each month, return the
#' p.values of the intercept and slope.
#'
#' @param m_params a tibble with monthly linear model parameters returned from
#' a call to `broom::tidy`.
#' @param prefix a character(1) for prefixing the new columns names.
#'
#' @return a tibble.
#'
get_pvalues_from_lm01_params <- function(m_params, prefix) {
  month <- pvalue_tb <- NULL
  if (nrow(m_params) == 0) {
    return(m_params)
  }
  res <- 
    m_params |>
    tidyr::nest(
      .by = month,
      .key = "m_params"
    ) |> 
    dplyr::mutate(
      pvalue_tb = purrr::map(
        .x = m_params,
        #NOTE:get_pvalues_from_lm12_params ignores the month column, making it
        #     reusable here.
        .f = get_pvalues_from_lm12_params,
        prefix = "lm01_"
      )
    ) |>
    tidyr::unnest(pvalue_tb)
  return(res)
}



#' Classify pvalues into categories
#'
#' @description
#' Given a data frame with a column with p.values, classify them into
#' categories.
#'
#' @param dat_tb a tiblbe with a column with p.values.
#' @param breaks_tb a tibble with the p.value categories.
#' @param pvalue_col a column with p.values in `dat_tb`.
#'
#' @return `dat_tb` with additional columns: `pinverval` and `pcolor` .
#'
#'@export
#'
classify_pvalues <- function(dat_tb, breaks_tb, pvalue_col) {
  pinterval <- NULL
  stopifnot(
    "Columns not found in breaks_tb!" =
    all(c("pbreaks", "pcolors", "pnames") %in% colnames(breaks_tb))
  )
  res <-
    dat_tb |>
    dplyr::mutate(
      pinterval = purrr::map_chr(
        #.x = {{pvalue_col}},
        .x = {{pvalue_col}},
        .f = find_interval_name,
        breaks = breaks_tb[["pbreaks"]],
        bnames = breaks_tb[["pnames"]]
      ),
      pinterval = factor(
        x = pinterval,
        levels = breaks_tb[["pnames"]],
        ordered = TRUE
      ),
      pcolor = breaks_tb[["pcolors"]][pinterval]
    )
  return(res)
}


#' #' Prepare data for pvalue plots
#' #'
#' #' @description
#' #' Prepare the given data for plotting their p-values.
#' #'
#' #' @param plot_data a tibble.
#' #' @param breaks_tb a tibble representing p-value intervals.
#' #'
#' #' @return a tibble; plot_data with additional columns.
#' #'
#' #' @export
#' #'
#' get_plot_data_pvalue <- function(plot_data, breaks_tb) {
#'   lm01_rsqr_adj <- pinterval <- NULL
#'   stopifnot(
#'     "Columns not found in breaks_tb!" =
#'     all(c("pbreaks", "pcolors", "pnames") %in% colnames(breaks_tb))
#'   )
#'   plot_data <-
#'     plot_data |>
#'     dplyr::mutate(
#'       pinterval = purrr::map_chr(
#'         .x = lm01_rsqr_adj,
#'         .f = find_interval_name,
#'         breaks = breaks_tb[["pbreaks"]],
#'         bnames = breaks_tb[["pnames"]]
#'       ),
#'       pinterval = factor(
#'         x = pinterval,
#'         levels = breaks_tb[["pnames"]],
#'         ordered = TRUE
#'       ),
#'       pcolor = breaks_tb[["pcolors"]][pinterval]
#'     )
#'   return(plot_data)
#' }
