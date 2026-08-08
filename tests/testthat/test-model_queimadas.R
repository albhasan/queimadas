test_that("get_sat_data works", {
  # TODO
})

test_that("fit_lm works", {
  # TODO
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
