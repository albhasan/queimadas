test_that("get_brazil works", {
  # TODO
})
test_that("get_brazil_year_month works", {
  # TODO
})
test_that("get_brazil_month works", {
  # TODO
})
test_that("get_sat_pairs works", {

  data_tb <-
    tibble::tribble(
      ~period, ~satelite, ~n,
      "1998-06", "ATSR",    444,
      "1998-06", "NOAA-12", 3551,
      "1998-07", "ATSR",    870,
      "1998-07", "NOAA-12", 8067,
      "1998-08", "ATSR",    5545,
      "1998-08", "NOAA-12", 35551,
      "1998-09", "ATSR",    5220,
      "1998-09", "NOAA-12", 41976,
      "1998-10", "ATSR",    2602,
      "1998-10", "NOAA-12", 23499
    )

  ref_satellite <- c(
    previous  = "NOAA-12",
    current   = "AQUA_M-T",
    candidate = "NPP-375-M",
    candidate = "NPP-375-T",
    candidate = "NPP-375-PM",
    candidate = "NPP-375",
    candidate = "ATSR"
  )

  expected_tb <-
    tibble::tribble(
      ~satelite_x, ~satelite_y,
      "NOAA-12", "ATSR"
    )

  expect_equal(
    object = get_sat_pairs(data_tb = data_tb, satellites = ref_satellite),
    expected = expected_tb
  )

})

