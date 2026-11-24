test_that("get_satellites works", {
  sat <- get_satellites()
  expect_true(inherits(x = sat, what = "character"))
  expect_equal(length(sat), 6)
})
