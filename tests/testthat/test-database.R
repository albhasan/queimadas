test_that("load_to_database works", {

  test_dir <- tempdir()
  test_db <- tempfile(
    pattern = "fake-queimadas_",
    tmpdir = test_dir,
    fileext = ".sqlite"
  )
  test_file <- tempfile(
    pattern = "fake-queimadas_",
    tmpdir = test_dir,
    fileext = ".csv"
  )

  #NOTE: The given file is not a CSV.
  expect_error(
    load_to_database(
      db_con = DBI::dbConnect(
        drv = RSQLite::SQLite(),
        dbname = test_db
      ),
      file_paths = "/fake-directory/file.wrongext",
      c_types = "ddTcccccdnncd",
      table_name = "fire_foci",
      disconnect = TRUE
    )
  )

  # An empty CSV if given, the database is created.
  file.create(test_file)
  suppressMessages(
    res <- load_to_database(
      db_con = DBI::dbConnect(
        drv = RSQLite::SQLite(),
        dbname = test_db
      ),
      file_paths = test_file,
      c_types = "ddTcccccdnncd",
      table_name = "fire_foci",
      disconnect = TRUE
    )
  )
  expect_true(file.exists(test_db))

  ##NOTE: The database already exists because of the test above.
  if (file.exists(test_db)) {
    expect_error(
      suppressWarning(
        load_to_database(
          db_con = DBI::dbConnect(
            drv = RSQLite::SQLite(),
            dbname = test_db
          ),
          file_paths = test_file,
          c_types = "ddTcccccdnncd",
          table_name = "fire_foci",
          disconnect = TRUE
        )
      )
    )
  }

})
