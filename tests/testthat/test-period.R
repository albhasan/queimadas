test_that("is_period_valid works", {
  expect_true(is_period_valid("2007-01"))
  expect_true(is_period_valid(c("2007-01", "2007-02")))
  expect_false(is_period_valid("2007-001"))
  expect_false(is_period_valid(c("2007-01", "2007-001")))
  expect_false(is_period_valid(NA))
  expect_false(is_period_valid(7))
  expect_false(is_period_valid(c("2007-01", NA)))
  expect_false(is_period_valid(NA_character_))
  expect_false(is_period_valid(x = character()))
  expect_false(is_period_valid(x = character(0)))
})

test_that("period_to_date and date_to_period works", {
  aperiod <- "2007-01"
  adate <- period_to_date(aperiod)
  expect_equal(
    adate,
    as.Date("2007-01-01")
  )
  newperiod <- date_to_period(adate)
  expect_equal(
    newperiod,
    aperiod
  )
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
  expect_error(object = get_month_from_period(x = NA_character_))
  expect_error(object = get_month_from_period(x = character()))
})
