library(queimadas)

library(bfast)
library(dplyr)
library(ggplot2)
library(logger)
library(lubridate)
library(readr)
library(sf)
library(stringr)
library(tidyr)

logger::log_threshold(INFO)

logger::log_info("Script 02_exploratory_data_analysis.R starting...")


#---- Configuration ----

logger::log_info("Reading configuration...")

sqlite_file <- "~/Documents/data/r_packages/queimadas/fire.sqlite"

table_name <- "fire_foci"
out_dir <- "/home/alber/Documents/github/slides/queimadas/slides/figures"

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

plot_size_a5_ls <- queimadas::get_paper_size(name = "A5", orientation = "ls")
plot_size_a4_ls <- queimadas::get_paper_size(name = "A4", orientation = "ls")


#---- Get the data ----

logger::log_info("Getting data...")

logger::log_info("Connecting to the database...")
db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)


#---- Data: Brazil by year and month ----

logger::log_info("Aggregating Brazilian data by year and month...")

brazil_ym_tb <-
  db_con |>
  get_brazil_year_month(table_name = table_name) |>
  dplyr::collect()

# TODO: Check if the AM/PM separation was already done.
# NOTE: There are only afternoon NPP-375 observations (NPP-375-PM).
# NOTE: In NPP-375D, the D stands for "Diurno".
sats <- sort(unique(brazil_ym_tb$satelite))
sats[stringr::str_starts(sats, "NPP")]


#---- Table of number of foci in Brazil per year-month and satellite ----

# logger::log_info("Creating aggregation result table...")
# Save data as CSV.
# brazil_ym_tb |>
#   add_sat_family() |>
#   tidyr::pivot_wider(
#     names_from = satelite,
#     values_from = n,
#     names_repair = "universal"
#   ) |>
#   dplyr::select(order(colnames(.))) |>
#   dplyr::relocate(period) |>
#   dplyr::arrange(period) |>
#   readr::write_csv(
#     file = file.path(out_dir, "brasil_satellite_year_month.csv")
#   )


#---- Get satellite pairs for analysis ----

logger::log_info("Getting satellite pairs for analysis...")

sat_tb <-
  brazil_ym_tb |>
  get_sat_pairs(satellites = ref_satellite)


#---- Plot foci for every combination of satellites by year and month in Brazil ----

logger::log_info("Aggregating data by month (no year)...")
brazil_m_tb <-
  db_con |>
  get_brazil_month(table_name = table_name) |>
  dplyr::collect()

logger::log_info(
  "Plotting montly data (no year) and their regression lines..."
)

plot_lm_brazil_month <-
  sat_tb |>
  dplyr::left_join(
    y = brazil_m_tb,
    by = c("satelite_x" = "satelite"),
    suffix = c("", "_x"),
    relationship = "many-to-many"
  ) |>
  dplyr::left_join(
    y = brazil_m_tb,
    by = c("satelite_y" = "satelite"),
    suffix = c("", "_y"),
    relationship = "many-to-many"
  ) |>
  dplyr::rename(
    "period_x" = "period",
    "n_x" = "n"
  ) |>
  dplyr::filter(period_x == period_y) |>
  dplyr::select(-period_y) |>
  dplyr::rename("period" = "period_x") |>
  dplyr::mutate(
    period = as.integer(period),
    period = as.factor(period)
  ) |>
  filter_complete_series(n_obs = 12) |>
  dplyr::arrange(satelite_x, satelite_y, period) |>
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
  width = plot_size_a5_ls[["width"]],
  height = plot_size_a5_ls[["height"]],
  units = plot_size_a5_ls[["units"]]
)


#---- Plot of correlation in montly data (no year) in Brazil ----

logger::log_info("Estimating correlation among monthly data (no year)...")

cor_data <-
  brazil_m_tb |>
  dplyr::filter(satelite %in% ref_satellite) |>
  tidyr::pivot_wider(
    names_from = satelite,
    values_from = n,
    names_repair = "universal"
  ) %>% # NOTE: Use magrittr's pipe to enable the "."" in the next line.
  dplyr::select(order(colnames(.))) |>
  dplyr::relocate(period) |>
  dplyr::arrange(period) |>
  dplyr::select(-period)

cor_mt <- stats::cor(cor_data)
min_cor <- min(cor_mt, na.rm = TRUE)
cor_mt[is.na(cor_mt)] <- 0

logger::log_info(
  "Plotting correlations between time series from different satellites..."
)
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
    limit = c(min_cor, 1),
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0.85
  )

ggplot2::ggsave(
  filename = file.path(out_dir, "plot_cor_brazil_month.png"),
  plot = plot_cor_brazil_month,
  width = plot_size_a5_ls[["width"]],
  height = plot_size_a5_ls[["height"]],
  units = plot_size_a5_ls[["units"]]
)

logger::log_info(
  "Writing table of overlapping-satellite correlations..."
)

ref_satellite |>
  utils::combn(m = 2) |>
  t() |>
  data.frame() |>
  magrittr::set_colnames(value = c("satelite_x", "satelite_y")) |>
  dplyr::mutate(
    cor_val = purrr::map2(
      .x = satelite_x,
      .y = satelite_y,
      .f = function(x, y, data_tb) {
        x_df <- dplyr::filter(data_tb, satelite == x)
        y_df <- dplyr::filter(data_tb, satelite == y)
        xy_df <- dplyr::inner_join(x_df, y_df, by = "period")
        if (nrow(xy_df) == 0) {
          return(NA)
        }
        return(
          round(
            x = cor(
              x = xy_df[["n.x"]],
              y = xy_df[["n.y"]],
              use = "everything",
              method = "pearson"
            ),
            digits = 3
          )
        )
      },
      data_tb = brazil_ym_tb
    )
  ) |>
  dplyr::filter(!is.na(cor_val)) |>
  get_latex_char() |>
  readr::write_csv(
    file = file.path(
      dirname(out_dir),
      "tables",
      "correlation_overlapping_ts.csv"
    )
  )


#---- Line plot ----

logger::log_info("Line plots...")

logger::log_info("Creaing break lines on August of each year...")
break_lines <-
  get_break_lines_year(
    data_tb = brazil_ym_tb,
    period_col = "period",
    break_pattern = "-08$"
  ) |>
  paste0("-01") |>
  lubridate::as_date()

brazil_ym_ref_tb <-
  brazil_ym_tb |>
  dplyr::filter(satelite %in% ref_satellite) |>
  dplyr::mutate(
    period = stringr::str_c(period, "-15"),
    period = lubridate::as_date(period)
  )

plot_line_brazil_year_month <-
  ggplot2::ggplot() +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      x = period,
      y = n,
      color = satelite,
      group = satelite
    ),
    data = brazil_ym_ref_tb
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = period,
      y = n,
      color = satelite,
      group = satelite
    ),
    data = brazil_ym_ref_tb
  ) +
  ggplot2::scale_x_continuous(breaks = break_lines) +
  ggplot2::theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggplot2::ggsave(
  filename = file.path(out_dir, "plot_line_brazil_year_month.png"),
  plot = plot_line_brazil_year_month,
  width = plot_size_a5_ls[["width"]],
  height = plot_size_a5_ls[["height"]],
  units = plot_size_a5_ls[["units"]]
)


#---- Peak analysis ----

year_peak_tb <-
  brazil_ym_ref_tb |>
  dplyr::mutate(y = stringr::str_sub(string = period, start = 1L, end = 4L)) |>
  dplyr::group_by(satelite, y) |>
  dplyr::slice(which.max(n)) |>
  dplyr::ungroup() |>
  dplyr::select(-y) |>
  tidyr::nest(.by = satelite) |>
  dplyr::mutate(
    lm_obj = purrr::map(
      .x = data,
      .f = function(x) {
        return(lm(formula = n ~ period, data = x))
      }
    ),
    lm_ci = purrr::map(
      .x = lm_obj,
      .f = predict_ci
    ),
    lm_ci = purrr::map2(
      .x = data,
      .y = lm_ci,
      .f = dplyr::bind_cols
    )
  ) |>
  dplyr::select(satelite, lm_ci) |>
  tidyr::unnest(lm_ci)

plot_line_brazil_year_month_facet <-
  plot_line_brazil_year_month +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      x = period,
      y = fit
    ),
    data = year_peak_tb,
    linetype = "dotted",
    alpha = 0.95
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = period,
      y = fit
    ),
    data = year_peak_tb,
    shape = 0,
    alpha = 0.85
  ) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(
      x = period,
      ymin = lwr,
      ymax = upr,
      group = satelite,
      alpha = 0.8
    ),
    data = year_peak_tb,
    fill = "gray85"
  ) +
  ggplot2::facet_wrap(
    facets = vars(satelite),
    scales = "free"
  )

ggplot2::ggsave(
  filename = file.path(out_dir, "plot_line_brazil_year_month_facet.png"),
  plot = plot_line_brazil_year_month_facet,
  width = plot_size_a5_ls[["width"]],
  height = plot_size_a5_ls[["height"]],
  units = plot_size_a5_ls[["units"]]
)

#---- Brazil data by state, year and month ----

logger::log_info("Aggregating Brazilian data by state, year, and month...")

brstate_tb <-
  queimadas::brstate_tb |>
  dplyr::select(estado = name_state, region = name_region) |>
  dplyr::mutate(
    estado = toupper(estado),
    estado = stringi::stri_trans_general(str = estado, id = "Latin-ASCII")
  )

brazil_state_ym_tb <-
  db_con |>
  get_brazil(table_name = table_name) |>
  dplyr::select(data_pas, satelite, estado) |>
  dplyr::filter(satelite %in% ref_satellite) |>
  dplyr::mutate(
    period = stringr::str_sub(string = data_pas, start = 1L, end = 7L)
  ) |>
  dplyr::summarize(
    n = dplyr::n(),
    .by = tidyselect::all_of(x = c("period", "satelite", "estado"))
  ) |>
  dplyr::arrange(period, satelite, estado) |>
  dplyr::collect()

brazil_region_ym_tb <-
  brazil_state_ym_tb |>
  dplyr::mutate(
    estado = toupper(estado),
    estado = stringi::stri_trans_general(str = estado, id = "Latin-ASCII")
  ) |>
  dplyr::left_join(
    y = brstate_tb,
    by = "estado"
  ) |>
  dplyr::summarize(
    n = sum(n),
    .by = tidyselect::all_of(x = c("period", "satelite", "region"))
  )

logger::log_info("Creating line plots of Brazil by region, year, and month...")
plot_line_brazil_state_year_month <-
  brazil_region_ym_tb |>
  dplyr::mutate(
    sat_family = dplyr::case_when(
      stringr::str_detect(string = satelite, pattern = "AQUA")
      ~ "AQUA-NOAA-NPP",
      stringr::str_detect(string = satelite, pattern = "NOAA")
      ~ "AQUA-NOAA-NPP",
      stringr::str_detect(string = satelite, pattern = "NPP")
      ~ "AQUA-NOAA-NPP",
      stringr::str_detect(string = satelite, pattern = "GOES")
      ~ "GOES",
      stringr::str_detect(string = satelite, pattern = "TERRA")
      ~ "TERRA",
      .default = "OTHER"
    )
  ) |>
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
    ncol = 3,
    scales = "free"
  )

ggplot2::ggsave(
  filename = file.path(out_dir, "plot_line_brazil_region_year_month.png"),
  plot = plot_line_brazil_state_year_month,
  width = plot_size_a4_ls[["height"]],
  height = plot_size_a4_ls[["width"]],
  units = plot_size_a4_ls[["units"]]
)


#---- BFast analysis ----

logger::log_info("Running BFast analysis...")

bfast_tb <-
  brazil_ym_tb |>
  dplyr::filter(satelite %in% ref_satellite) |>
  dplyr::mutate(
    ymd = stringr::str_c(period, "-15"),
    ymd = lubridate::as_date(ymd)
  ) |>
  dplyr::select(ymd, n, satelite) |>
  tidyr::nest(.by = satelite) |>
  dplyr::mutate(
    ts = purrr::map(
      .x = data,
      .f = function(data_tb) {
        # NOTE: BFast demand data to have at least two periods.
        if (nrow(data_tb) < 25) {
          return(NA)
        }
        # NOTE: BFast demand data to be periodic.
        data_tb <- data_tb[1:(nrow(data_tb) %/% 12 * 12), ]
        ts_obj <- stats::ts(
          data = data_tb[["n"]],
          frequency = 12,
          start = c(year(data_tb[[1, "ymd"]]), month(data_tb[[1, "ymd"]])),
          end = c(
            year(data_tb[[nrow(data_tb), "ymd"]]),
            month(data_tb[[nrow(data_tb), "ymd"]])
          )
        )
        return(ts_obj)
      }
    )
  ) |>
  dplyr::mutate(
    bf = purrr::map(
      .x = ts,
      .f = function(x) {
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
    )
  )

logger::log_info("Saving BFast plots to disk...")

for (i in seq_len(nrow(bfast_tb))) {
  sat_name <- bfast_tb[["satelite"]][[i]]
  filename <- file.path(
    out_dir,
    paste0("plot_bfast_brazil_year_month_", sat_name, ".png")
  )
  bf_obj <- bfast_tb[["bf"]][[i]]
  if (!all(is.na(bf_obj))) {
    logger::log_info("Saving BFast plot to ", basename(filename), "...")
    grDevices::png(
      filename = filename,
      width = plot_size_a5_ls[["width"]],
      height = plot_size_a5_ls[["height"]],
      units = plot_size_a5_ls[["units"]],
      res = 72
    )
    plot(bf_obj)
    dev.off()
  }
}


#---- Disconnect from the database ----

logger::log_info("Disconnecting from the database...")
DBI::dbDisconnect(conn = db_con)

logger::log_info("Script 02_exploratory_data_analysis.R finished!")
