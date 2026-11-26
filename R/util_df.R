#' Add the list's names to the data frames in the list
#'
#' @description
#' Given a list of data frames, add each list's name to its corresponding data
#' frame.
#'
#' @param df_ls a named list of data frames.
#' @param cname a character(1). The name for the new column.
#'
#' @return         A list of data frames or a list of NAs.
#'
#' @export
#'
lnames2df <- function(df_ls, cname) {

  all_na <-
    all(vapply(
      X = df_ls,
      FUN = function(x) {
        if(length(x) == 1 && is.na(x)) {
          return(TRUE)
        }
        return(FALSE)
      },
      FUN.VALUE = logical(1)
    ))
  if(all_na) {return(df_ls)}

  stopifnot(
    "Expected a list of data frames!" =
      all(vapply(
        X = df_ls,
        FUN = is.data.frame,
        FUN.VALUE = logical(1)
      ))
  )
  stopifnot(
    "Expected a named list!" =
      length(names(df_ls)) == length(df_ls)
  )

  res <- lapply(
    X = seq(df_ls),
    FUN = function(x, df_ls) {
      df_ls[[x]][cname] <- names(df_ls)[x]
      return(df_ls[[x]])
    },
    df_ls = df_ls
  )

  return(res)
}



#' Check a data frame has data
#'
#' @description
#' Utilitary function. Return true if the given data frame is not NA.
#'
#' @param plot_lm01_data a data frame or NA.
#'
#' @return a logical(1). FALSE if the given parameter is actually an NA. TRUE
#' otherwise.
#'
#' @export
#'
has_df_data <- function(plot_lm01_data) {
  if (length(plot_lm01_data) == 1 && is.na(plot_lm01_data)) {
    return(FALSE)
  }
  return(TRUE)
}
