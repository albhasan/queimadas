library(devtools)
devtools::load_all()

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(sf)
library(stringr)
library(tidyr)



#---- Configuration ----

sqlite_file <- "~/Downloads/fire.sqlite"
table_name <- "fire_foci"
out_dir <- "~/Downloads/tmp/fire_results"

stopifnot("Database file not found!" = file.exists(sqlite_file))
stopifnot("Directory not found!" = dir.exists(out_dir))

ref_satellite <- c(
  previous = "NOAA-12",
  current = "AQUA_M-T",
  candidate = "NPP-375-M",
  candidate = "NPP-375-T",
  candidate = "NPP-375-PM",
  candidate = "NPP-375",
  candidate = "NPP-375D"
)



#---- Utilitary functions ----

# Filter complete series
#
# @description
# Keep those series with the complete number observations for a period.
#
# @param x a tibble with the variables period, satelite, and n.
# @param n_obs number of observations in a period.
#
# @return a modified version of x.
#
filter_complete_series <- function(x, n_obs) {
  # Find satellits' combinations with 12 observations.
  complete_series <-
    x %>%
    dplyr::summarize(
      n_period = dplyr::n(),
      .by = tidyselect::all_of(c("satelite_x", "satelite_y"))
    ) %>%
    dplyr::filter(n_period >= n_obs) %>%
    dplyr::select(-n_period) %>%
    dplyr::mutate(
      sat_key = stringr::str_c(satelite_x, satelite_y, sep = "-")
    ) %>%
    dplyr::select(sat_key)
  # Filter only series with at least 12 observations.
  x %>%
    dplyr::mutate(
      sat_key = stringr::str_c(satelite_x, satelite_y, sep = "-")
    ) %>%
    dplyr::right_join(
      y = complete_series,
      by = "sat_key"
    ) %>%
    dplyr::select(-sat_key) %>%
    return()
}



# Add the family to which the satelite belongs to
#
# @param x a tibble
#
# @return x with a new column called sat_family.
#
add_sat_family <- function(x) {
  x %>%
  dplyr::mutate(
    sat_family = dplyr::case_when(
      stringr::str_detect(string = satelite, pattern = "AQUA") ~ "AQUA",
      stringr::str_detect(string = satelite, pattern = "GOES") ~ "GOES",
      stringr::str_detect(string = satelite, pattern = "NOAA") ~ "NOAA",
      stringr::str_detect(string = satelite, pattern = "NPP") ~ "NPP",
      stringr::str_detect(string = satelite, pattern = "TERRA") ~ "TERRA",
      .default = "OTHER"
    )
  ) %>%
  return()
}



#---- Get the data ----

# Connect to the database.
db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)
foci_tb <- dplyr::tbl(db_con, table_name)



#---- Prepare the data ----

foci_tb <-
  db_con %>%
  dplyr::tbl(table_name) %>%
  dplyr::filter(
    pais == "Brasil",
    id_area_industrial == "0"
  ) %>%
  dplyr::mutate(
    afternoon = lubridate::hour(lubridate::as_datetime(data_pas)) >= 12
  )



#---- Data: Brazil by year and month ----

brazil_ym_tb <-
  foci_tb %>%
   dplyr::mutate(
     period = stringr::str_sub(string = data_pas, start = 1L, end = 7L),
     satelite = dplyr::if_else(
       condition = satelite == "NPP-375" & afternoon == 1,
       true = "NPP-375-PM",
       false = satelite
     ),
     satelite = dplyr::if_else(
       condition = satelite == "NPP-375" & afternoon == 0,
       true = "NPP-375-AM",
       false = satelite
     )
   ) %>%
  dplyr::select(satelite, period) %>%
  dplyr::summarize(
    n = dplyr::n(),
    .by = tidyselect::all_of(x = c("period", "satelite"))
  ) %>%
  dplyr::arrange(period, satelite) %>%
  dplyr::collect()

# TODO: Check if the AM/PM separation was already done.
# NOTE: There are only afternoon NPP-375 observations (NPP-375-PM).
# TODO: What is NPP-375D?
sats = sort(unique(brazil_ym_tb$satelite))
sats[stringr::str_starts(sats, "NPP")]


#---- Table of number of foci in Brazil per year-month and satellite ----

# Save data as CSV.
brazil_ym_tb %>%
  add_sat_family() %>%
  tidyr::pivot_wider(
    names_from = satelite,
    values_from = n,
    names_repair = "universal"
  ) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::relocate(period) %>%
  dplyr::arrange(period) %>%
  readr::write_csv(
    file = file.path(out_dir, "brasil_satellite_year_month.csv")
  )



#---- Combinatory of satellites ----


sat_tb <-
  brazil_ym_tb %>%
  dplyr::select(satelite) %>%
  dplyr::distinct(satelite) %>%
  dplyr::filter(satelite %in% ref_satellite) %>%
  dplyr::pull(satelite) %>%
  expand_grid(satelite_x = ., satelite_y = .) %>%
  dplyr::filter(
    satelite_x != satelite_y,
    satelite_y %in% ref_satellite[names(ref_satellite) == "candidate"]
  ) %>%
  dplyr::arrange(satelite_y)



#---- Plot foci for every combination of satellites by year and month in Brazil ----

# Brazil data by month (no year).
brazil_m_tb <-
  brazil_ym_tb %>%
  dplyr::mutate(
      period = stringr::str_sub(string = period, start = 6L, end = 7L)
  ) %>%
  dplyr::summarize(
    n = sum(n, na.rm = FALSE),
    .by = tidyselect::all_of(x = c("period", "satelite"))
  )

# Plot of points of montly data (no year) in Brazil with regression lines.
plot_lm_brazil_month <-
  sat_tb %>%
  dplyr::left_join(
    y = brazil_m_tb,
    by = c("satelite_x" = "satelite"),
    suffix = c("", "_x"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    y = brazil_m_tb,
    by = c("satelite_y" = "satelite"),
    suffix = c("", "_y"),
    relationship = "many-to-many"
  ) %>%
  dplyr::rename(
    "period_x" = "period",
    "n_x" = "n"
  ) %>%
  dplyr::filter(period_x == period_y) %>%
  dplyr::select(-period_y) %>%
  dplyr::rename("period" = "period_x") %>%
  dplyr::mutate(
    period = as.integer(period),
    period = as.factor(period)
  ) %>%
  filter_complete_series(n_obs = 12) %>%
  dplyr::arrange(satelite_x, satelite_y, period) %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = n_x,
      y = n_y
    )
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      group = period,
      color = period
    )
  ) +
  ggplot2::geom_smooth(
    formula = y ~ x,
    method = "lm",
    level = 0.95
  ) +
  ggplot2::geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dotted"
  ) +
  ggplot2::coord_fixed(ratio = 1) +
  ggplot2::facet_grid(
    rows = satelite_y ~ satelite_x
  ) +
  ggplot2::theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggplot2::ggsave(
  filename = file.path(out_dir, "plot_lm_brazil_month.png"),
  plot = plot_lm_brazil_month,
  width = 297,
  height = 210,
  units = "mm"
)



#---- Plot of correlation in montly data (no year) in Brazil ----

cor_data <- 
  brazil_m_tb %>%
  dplyr::filter(satelite %in% ref_satellite) %>%
  tidyr::pivot_wider(
    names_from = satelite,
    values_from = n,
    names_repair = "universal"
  ) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::relocate(period) %>%
  dplyr::arrange(period) %>%
  dplyr::select(-period)

cor_mt <- stats::cor(cor_data)
min_cor <- min(cor_mt, na.rm = TRUE)
cor_mt[is.na(cor_mt)] <- 0

plot_cor_brazil_month <-
  ggcorrplot::ggcorrplot(
    corr = cor_mt,
    method = "square",
    type = "lower",
    outline.col = "white",
    lab = TRUE,
    hc.order = TRUE
  ) +
  scale_fill_gradient2(
    limit = c(min_cor ,1),
    low = "blue",
    high =  "red",
    mid = "white",
    midpoint = 0.85
  )

ggplot2::ggsave(
  filename = file.path(out_dir, "plot_cor_brazil_month.png"),
  plot = plot_cor_brazil_month,
  width = 297 * 2,
  height = 210 * 2,
  units = "mm"
)



#---- Line plot ----

# Break lines at Auguts of each year in the data.
break_lines <-
  brazil_ym_tb %>%
  dplyr::select(period) %>%
  dplyr::distinct(period) %>%
  dplyr::filter(stringr::str_detect(string = period, pattern = "-08$")) %>%
  dplyr::pull(period)

plot_line_brazil_year_month <-
  brazil_ym_tb %>%
  dplyr::filter(satelite %in% ref_satellite) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(
    x = period,
    y = n,
    color = satelite,
    group = satelite
  )) +
  ggplot2::geom_point(ggplot2::aes(
    x = period,
    y = n,
    color = satelite,
    group = satelite
  )) +
  ggplot2::scale_x_discrete(breaks = break_lines) +
  ggplot2::theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggplot2::ggsave(
  filename = file.path(out_dir, "plot_line_brazil_year_month.png"),
  plot = plot_line_brazil_year_month,
  width = 297,
  height = 210,
  units = "mm"
)



#---- Brazil data by state, year and month ----

brstate_tb <-
  geobr::read_state() %>%
  sf::st_drop_geometry() %>%
  dplyr::select(estado = name_state, region = name_region) %>%
  dplyr::mutate(
    estado = toupper(estado),
    estado = stringi::stri_trans_general(str = estado, id = "Latin-ASCII")
  )

brazil_state_ym_tb <-
  foci_tb %>%
  dplyr::select(data_pas, satelite, estado) %>%
  dplyr::filter(satelite %in% ref_satellite) %>%
  dplyr::mutate(
      period = stringr::str_sub(string = data_pas, start = 1L, end = 7L)
  ) %>%
  dplyr::summarize(
    n = dplyr::n(),
    .by = tidyselect::all_of(x = c("period", "satelite", "estado"))
  ) %>%
  dplyr::arrange(period, satelite, estado) %>%
  dplyr::collect()

brazil_region_ym_tb <-
  brazil_state_ym_tb %>%
  dplyr::mutate(
    estado = toupper(estado),
    estado = stringi::stri_trans_general(str = estado, id = "Latin-ASCII")
  ) %>%
  dplyr::left_join(
    y = brstate_tb,
    by = "estado"
  ) %>%
  dplyr::summarize(
    n = sum(n),
    .by = tidyselect::all_of(x = c("period", "satelite", "region"))
  )

break_lines <-
  brazil_region_ym_tb %>%
  dplyr::select(period) %>%
  dplyr::distinct(period) %>%
  dplyr::filter(stringr::str_detect(string = period, pattern = "-08$")) %>%
  dplyr::pull(period)

plot_line_brazil_region_year_month <-
  brazil_region_ym_tb %>%
  dplyr::mutate(
    sat_family = dplyr::case_when(
      stringr::str_detect(string = satelite, pattern = "AQUA") ~ "AQUA-NOAA-NPP",
      stringr::str_detect(string = satelite, pattern = "NOAA") ~ "AQUA-NOAA-NPP",
      stringr::str_detect(string = satelite, pattern = "NPP") ~  "AQUA-NOAA-NPP",
      stringr::str_detect(string = satelite, pattern = "GOES") ~ "GOES",
      stringr::str_detect(string = satelite, pattern = "TERRA") ~ "TERRA",
      .default = "OTHER"
    )
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(
    x = period,
    y = n,
    group = satelite,
    color = satelite
  )) +
  ggplot2::scale_x_discrete(breaks = break_lines) +
  ggplot2::theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  ggplot2::facet_wrap(
    facets = dplyr::vars(region),
    ncol = 1,
    scales = "free"
  )

ggplot2::ggsave(
  filename = file.path(out_dir, "plot_line_brazil_region_year_month.png"),
  plot = plot_line_brazil_state_year_month,
  width = 297 * 4,
  height = 210 * 4,
  units = "mm"
)
