#' Get the file name from a path
#'
#' @description
#' Get the file's name (without path nor extension) from the given file path.
#'
#' @param file_path a character(1). A path to a file.
#'
#' @export
#'
get_base_file_name <- function(file_path) {
  return(tools::file_path_sans_ext(basename(file_path)))
}



#' Get the inteval's name
#'
#' @description
#' Given a value and an sorted numeric of breaks, return the name corresponding
#' to x's interval.
#'
#' @param x a numeric.
#' @param breaks a numeric (see `base::findInterval`)
#' @param bnames a character. The names for each interval defined by the given
#' breaks.
#'
#' @export
#'
find_interval_name <- function(x, breaks, bnames) {
  #TODO: Should I rename this function to get_name_in_interval?
  return(bnames[findInterval(x = x, vec = breaks)])
}


#' Test if the object is made of NAs
#'
#' @description
#' Test if the given object is either NA or a list made of NAs.
#'
#' @param x an atomic object or a list of them.
#'
#' @return a logical or a list of them.
#'
#' @export
#'
is_made_of_na <- function(x) {
  if(is.atomic(x)) {
    return(
      all(is.na(x))
    )
  } else if (
    inherits(x = x, what = "list") ||
    inherits(x = x, what = "data.frame")
  ) {
    return(
      all(vapply(
        X = x,
        FUN = is_made_of_na,
        FUN.VALUE = logical(1)
      ))
    )
  } 
  return(FALSE)
}
