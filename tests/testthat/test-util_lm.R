test_that("get_lm_equation works", {
  #TODO:
})

test_that("get_lm_r2 works", {
  lm_ls <- unlist(
    x = test_lm[["lm_01_models"]],
    recursive = FALSE
  )

  rs_adj <- double(length(lm_ls))
  rs_nadj <- double(length(lm_ls))
  for (i in seq_along(lm_ls)) {
    rs_adj[i] <- get_lm_r2(lm_obj = lm_ls[[i]], adjusted = TRUE)
    rs_nadj[i] <- get_lm_r2(lm_obj = lm_ls[[i]], adjusted = FALSE)
  }

  expect_equal(length(rs_adj), length(rs_nadj))
  expect_equal(class(rs_adj), class(rs_nadj))
  expect_true(inherits(x = rs_adj, what = "numeric"))
  expect_equal(sum(is.na(rs_adj)), 5)
  expect_equal(sum(is.nan(rs_adj)), 5)
  expect_equal(sum(is.na(rs_nadj)), 1)
  expect_equal(sum(is.nan(rs_nadj)), 1)

  # Test NA
  expect_equal(get_lm_r2(lm_obj = NA, adjusted = FALSE), NA)
  expect_equal(
    object = get_lm_r2(lm_obj = list(lm_ls[[1]], NA), adjusted = FALSE),
    expected = c(1, NA)
  )
  expect_equal(
    object = get_lm_r2(lm_obj = list(NA, lm_ls[[1]], NA), adjusted = FALSE),
    expected = c(NA, 1, NA)
  )

  # Test error.
  expect_error(get_lm_r2(lm_obj = 9, adjusted = FALSE))
  expect_error(get_lm_r2(lm_obj = 9L, adjusted = FALSE))
  expect_error(get_lm_r2(lm_obj = "9", adjusted = FALSE))

  # Test vectorization.
  rsv_adj <- get_lm_r2(lm_obj = lm_ls, adjusted = TRUE)
  rsv_nadj <- get_lm_r2(lm_obj = lm_ls, adjusted = FALSE)
  names(rsv_adj) <- names(rsv_nadj) <- NULL

  expect_equal(object = rsv_adj,  expected = rs_adj)
  expect_equal(object = rsv_nadj, expected = rs_nadj)

})

test_that("get_lm_residuals works", {
  #TODO:
})

test_that("predict_ci works", {
  #TODO:
})


