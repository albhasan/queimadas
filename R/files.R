#' Unzip files
#'
#' @description
#' Unzip the given zip files into the given directory.
#'
#' @param files a character. Path to zip files.
#' @param out_dir a character(1). Path to a directory.
#'
#' @return a character. Path to the unzipped files. The vector names are the
#'   input zip files.
#'
#' @export
#'
unzip_files <- function(files, out_dir) {

  stopifnot("File not found!" = all(sapply(files, file.exists)))
  stopifnot("Output directory not found!" = dir.exists(out_dir))

  ex_files <-
    vapply(
           X = files,
           FUN = utils::unzip,
           exdir = out_dir,
           FUN.VALUE = character(1)
    )

  return(ex_files)

}
