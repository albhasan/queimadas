#' Download data from the Queimadas Program
#'
#' @description
#' Download data files from the Queimadas Program.
#'
#' @param out_dir a character(1). Path to a directory for storing the
#'   downloaded data.
#' @param data_url a character(1). The URL of the Web page where the data is
#'   hosted.
#' @param wait_time a numeric(1). Number of seconds to wait between requests
#'   for files.
#' @param overwrite_files a logical(1). Should the downloaded files overwrite
#'   older versions?
#' @param quiet a logical(1). Should status messages be displayed during
#'   download?
#'
#' @return a character. Full path to the downloaded files.
#'
#' @export
#'
download_data <- function(out_dir, data_url = get_data_url(), wait_time = 5,
                          overwrite_files = FALSE, quiet = FALSE) {

  stopifnot("Output directory not found!" = dir.exists(out_dir))

  # Get the zip names.
  zip_names <-
    url |>
    rvest::read_html() |>
    rvest::html_nodes(xpath = ".//a[contains(@href, '.zip')]") |>
    rvest::html_attr("href")

  # Convert file names to urls.
  zip_urls <-
    file.path(data_url, zip_names)

  # Download.
  zip_files <- 
    purrr::walk(
      .x = zip_urls, 
      .f = ~{
        if (!quiet)
          message("Downloading: ", .x)
        httr::GET(
          url = .x,
          httr::write_disk(
            path = file.path(out_dir, basename(.x)),
            overwrite = overwrite_files
          )
        )
        Sys.sleep(wait_time)
      }
    )

  return(
    file.path(out_dir, basename(zip_files))
  )

}



#' Get the data URL
#'
#' @description
#' Get the URL of the Web Page that holds the links to the data files.
#'
#' @return a character(1). An URL.
#'
#' @export
#'
get_data_url <- function() {
  #NOTE: This directory includes data from all the satellites only for Brazil.
  #NOTE: Data for south America includes only the reference satellite and it
  # is a couple of leves up from Brazil's data.
  return("https://dataserver-coids.inpe.br/queimadas/queimadas/focos/csv/anual/Brasil_todos_sats")
}



