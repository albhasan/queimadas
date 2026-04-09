test_that("get_sat_data works", {
  # TODO
})

test_that("fit_lm works", {
  # TODO
})

test_that("get_month_from_period works", {
  expect_equal(
    object = get_month_from_period(x = "2008-07"),
    expected = "07"
  )
  expect_equal(
    object = get_month_from_period(x = c("2008-07", "2007-01")),
    expected = c("07", "01")
  )
  expect_error(object = get_month_from_period(x = "2008"))
  expect_error(object = get_month_from_period(x = "20081-02342"))
  expect_error(object = get_month_from_period(x = "200808"))
  expect_error(object = get_month_from_period(x = c("2008-08", "2006")))
  expect_error(object = get_month_from_period(x = 189L))
  expect_error(object = get_month_from_period(x = character()))
  expect_error(object = get_month_from_period(x = NA_character_))
})
