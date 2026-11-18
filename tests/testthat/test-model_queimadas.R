test_that("get_sat_data works", {
  # TODO:
})

test_that("get_sat_data_cell works", {
  # TODO:
})

test_that("fit_lm_12_months works", {
  # TODO:
})

test_that(" fit_lm_01_months", {
  # NOTE: fit_lm_01_months finds the overlapping y~x observations by itself.

  # Get test data.
  x_df <- test_ts[test_ts$satelite == "AQUA_M-T", c("period", "n")]
  y_df <- test_ts[test_ts$satelite == "NOAA-12", c("period", "n")]
  colnames(x_df) <- c("period", "x")
  colnames(y_df) <- c("period", "y")

  # NOTE: There are some month with just a few points for fitting a model.
  month_lm_ls <-
    fit_lm_01_months(
      x_df = x_df, 
      y_df = y_df,
      formula = "y ~ x",
      clevel = 0.95
    )

  # Expect NA when there ir no data, or not enough data to run a regression.
  expect_equal(
    object = month_lm_ls[[1]],
    expected = NA
  )
  expect_equal(
    object = month_lm_ls[[2]],
    expected = NA
  )
  expect_equal(
    object = month_lm_ls[[3]],
    expected = NA
  )
  expect_equal(
    object = month_lm_ls[[4]],
    expected = NA
  )
  expect_equal(
    object = month_lm_ls[[5]],
    expected = NA
  )
  expect_equal(
    object = month_lm_ls[[12]],
    expected = NA
  )

  # Expect one regression for each month of the year.
  expect_equal(
    object = length(month_lm_ls),
    expected = 12L
  )

  # Check the estructure of the list.
  expect_equal(
    object = names(month_lm_ls[[6]]),
    expected = c("model", "data")
  )
  expect_true(inherits(month_lm_ls[[6]][["model"]], what = "lm"))
  expect_true(inherits(month_lm_ls[[6]][["data"]], what = "data.frame"))

  # Check the column names of the data frame with the regression results.
  expect_equal(
    object = colnames(month_lm_ls[[6]][["data"]]),
    expected = c( "year", "m", "x", "y", "fit",
                 "lwr", "upr","residuals","is_outlier")
  )

})

test_that("fit_lm works", {
  # TODO:
})

test_that("overlap_len works", {
  # TODO:
})

test_that("get_ts_overlap works", {
  x <- sample(x = 1:49, size = 1):sample(x = 51:99, size = 1)
  y <- sample(x = 1:49, size = 1):sample(x = 51:99, size = 1)
  x_df <- data.frame(period = x)
  y_df <- data.frame(period = y)
  res <- get_ts_overlap(x_df = x_df, y_df = y_df, cname = "period")
  minmax_df <- data.frame(period = max(min(x), min(y)):min(max(x), max(y)))
  expect_equal(
    object = res,
    expected = minmax_df
  )

  y_df <- data.frame(badcolname = y)
  expect_error(
    get_ts_overlap(x_df = x_df, y_df = y_df, cname = "period")
  )
})

test_that("get_ts_overlap works with real data", {
#TODO:
})

test_that("predict_ci_01 works", {
  lm_ls <- test_lm[["lm_01_models"]][[1]]
  x_df <- test_lm[["x_df"]][[1]]
  #NOTE: Some models are missing. There wasn't enough data to produce a model
  #      for each month.
  #NOTE: These tests failed after updating the test data!
  #expect_true(length(lm_ls) == 10)
  #expect_true(sum(is.na(lm_ls)) == 0)
  expect_equal(
    object = length(lm_ls),
    expected = 12
  )
  expect_equal(
    object = sum(is.na(lm_ls)),
    expected = 6
  )
  expect_error(
    predict_ci_01(
      lm_ls = lm_ls,
      new_data = x_df,
      clevel = 0.95
    )
  )

  lm_ls <- test_lm[["lm_01_models"]][[2]]
  x_df <- test_lm[["x_df"]][[2]]
  expect_true(length(lm_ls) == 12)
  expect_true(sum(is.na(lm_ls)) == 0)
  res <-
    predict_ci_01(
      lm_ls = lm_ls,
      new_data = x_df,
      clevel = 0.95
    )
  expect_true(inherits(x = res, what = "list"))
  expect_true(length(res) == 12)
  expect_true(sum(is.na(res)) == 0)
  expect_true(all(vapply(
    X = res,
    FUN = inherits,
    FUN.VALUE = logical(1),
    what = "data.frame"
  )))

  lm_ls <- test_lm[["lm_01_models"]][[2]]
  lm_ls[[1]] <- NA
  x_df <- test_lm[["x_df"]][[2]]
  expect_true(length(lm_ls) == 12)
  expect_true(sum(is.na(lm_ls)) == 1)
  expect_error(
    predict_ci_01(
      lm_ls = lm_ls,
      new_data = x_df,
      clevel = 0.95
    )
  )
})
