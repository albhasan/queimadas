#' Add the list's names to the data frames in the list
#'
#' @description
#' Given a list of data frames, add each list's name to its corresponding data
#' frame.
#'
#' @param df_ls a named list of data frames.
#' @param cname a character(1). The name for the new column.
#'
#' @return         A list of data frames.
#'
#' @export
#'
lnames2df <- function(df_ls, cname) {
  stopifnot(
    "Expected a list of data frames!" =
      all(vapply(df_ls, is.data.frame, logical(1)))
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
