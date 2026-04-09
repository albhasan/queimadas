#' Convert a data frame into a LaTeX-sanitized character data frame
#'
#' @description
#' Convert a data frame to another in which the contents are converted into
#' character vectors compatible with LaTeX.
#'
#' @param x a data frame.
#'
#' @return a character data frame.
#'
#' @export
#'
get_latex_char <- function(x) {
  res <-
    as.data.frame(apply(
      X = x,
      MARGIN = c(1, 2),
      FUN = Hmisc::latexTranslate
    ))
  colnames(res) <- Hmisc::latexTranslate(colnames(x))
  return(res)
}
