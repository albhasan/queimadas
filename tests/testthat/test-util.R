test_that("get_base_file_name works", {
  fnames <- 
    c(
      "/home/alber/Documents/results/r_packages/queimadas/cell_ts/active-fires_233_067.RDS",
      "~/Documents/results/r_packages/queimadas/cell_ts/active-fires_233_067.RDS",
      "~/Documents/results/r_packages/queimadas/cell_ts/active-fires_233_067"
    )
  exp <- c("active-fires_233_067", "active-fires_233_067", "active-fires_233_067")
  res <- character(length = length(fnames))
  for (i in seq_along(fnames)) {
    res[i] <- get_base_file_name(fnames[i])
  }

  expect_equal(
    object = res,
    expected = exp
  )
})

test_that("find_interval_name works", {
  #TODO:
})
