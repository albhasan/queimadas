test_that("get_data_url works", {
  data_url <- get_data_url()
  expect_length(object = data_url, n = 1)
  expect_type(object = data_url, type = "character")
  expect_identical(
    object = data_url,
    expected = "https://dataserver-coids.inpe.br/queimadas/queimadas/focos/csv/anual/Brasil_todos_sats"
  )


  # Test if the given URL is valid.
  # Adapted from https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
  is_url_valid <- function(url_in, t = 2){
    con <- url(url_in)
    check <- suppressWarnings(
      try(
          open.connection(
            con = con,
            open = "rt",
            timeout = t
          ),
        silent = TRUE
      )[1]
    )
    suppressWarnings(
      try(
        close.connection(con = con),
        silent = TRUE
      )
    )
    return(ifelse(is.null(check), TRUE, FALSE))
  }

  skip_on_cran()
  skip_if_offline()
  expect_true(is_url_valid(get_data_url()))

})



test_that("download_data works", {
  skip_on_cran()
  skip_if_offline()

  expect_error(download_data(out_dir = "/my/fake/dir"))
  # TODO: write more tests!
})
