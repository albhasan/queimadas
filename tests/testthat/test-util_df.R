test_that("lnames2df works", {

  df_ls <-
    list(
      model_01 = data.frame(x = rnorm(10), y = rnorm(10)),
      model_02 = data.frame(x = rnorm(10), y = rnorm(10)),
      model_03 = data.frame(x = rnorm(10), y = rnorm(10))
    )

  # Missing cnames.
  expect_error(lnames2df(df_ls))

  # Add a column to each data frame with their corresponding list's element.
  res <-
    lnames2df(
      df_ls = df_ls,
      cname = "c1"
    )

  expect_true(
    all(vapply(
      X = res, 
      FUN = inherits,
      FUN.VALUE = logical(1),
      what = "data.frame",
    ))
  ) 

  expect_equal(
    object = unique(vapply(
      X = res,
      FUN = function(x) {ncol(x)},
      FUN.VALUE = integer(1)
    )),
    expected = 3
  )
  expect_equal(
    object = unique(vapply(
      X = res,
      FUN = function(x) {nrow(x)},
      FUN.VALUE = integer(1)
    )),
    expected = 10 
  )
  expect_equal(
    object = unique(as.vector(vapply(
      X = res,
      FUN = function(x) {colnames(x)},
      FUN.VALUE = character(3)
    ))),
    expected = c("x", "y", "c1")
  )
  expect_equal(
    object = unique(vapply(
      X = res,
      FUN = function(x) {colnames(x)[3]},
      FUN.VALUE = character(1)
    )),
    expected = "c1"
  )

  df_ls <-
    list(
      model_01 = NA,
      model_02 = NA,
      model_03 = NA,
      model_04 = NA,
      model_05 = NA,
      model_06 = NA,
      model_07 = NA,
      model_08 = NA,
      model_09 = NA,
      model_10 = NA,
      model_11 = NA,
      model_12 = NA
    )
  res <-
    lnames2df(
      df_ls = df_ls,
      cname = "c1"
    )
  expect_equal(
    object = res,
    expected = df_ls
  )

})
