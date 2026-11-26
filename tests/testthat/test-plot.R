test_that("get_plot_forecast_vs_obs works", {
  # TODO
})

test_that("visual_test_ts works", {
  # TODO:
})

test_that("component_analysis works", {
  # TODO:
})

test_that("test_normal_residuals works", {
  # TODO:
})

test_that("test_normal_residuals works", {
  # TODO:/
})

test_that("get_plot_queimadas_forecast works", {
  res <-
    get_plot_queimadas_forecast(
      x_df = NA,
      y_df = NA, 
      forecast_df = data.frame(c1 = NA, c2 = NA, c3 = NA)
    )
  expect_equal(
    object = res,
    expected = NA
  )
})

test_that("get_plot_ref_sats works", {
  #TODO:
})

test_that("get_plot_ref_sats_01 works", {
  #TODO:
})

test_that("get_plot_ts_with_bfast_break works", {
  #TODO:
})

test_that("get_base_map works", {
  #TODO:
})

test_that("get_base_map works", {
  #TODO:
})
