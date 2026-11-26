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

test_that("is_made_of_na works", {
  expect_true(is_made_of_na(NA))
  expect_true(is_made_of_na(c(NA, NA, NA, NA, NA)))

  x = list(one = NA, two = NA, three = NA)
  expect_true(is_made_of_na(x))

  x = data.frame(one = NA, two = NA, three = NA)
  expect_true(is_made_of_na(x))

  expect_false(is_made_of_na(1))
  expect_false(is_made_of_na(c(1, NA)))
  expect_false(is_made_of_na(c(NA, 1, NA)))
  expect_false(is_made_of_na(list(1, list(NA))))
  expect_false(is_made_of_na(list(1, list(1, NA))))

  expect_false(is_made_of_na(data.frame(NA, 1, NA)))
  expect_false(is_made_of_na(data.frame(1, list(NA))))
  expect_false(is_made_of_na(data.frame(1, list(1, NA))))

  x = list(
    anlm = lm(formula = "x ~ y", data = data.frame(x = rnorm(10), y = rnorm(10))),
    another = NA
  )
  expect_false(is_made_of_na(x))
})
