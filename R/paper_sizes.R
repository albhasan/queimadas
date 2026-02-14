#' Get paper size
#'
#' @description
#' Get the dimensions of a international standard paper size.
#'
#' @param name name of the paper size (e.g. "A4").
#' @param orientation either landscape (ls) or portrait (pt).
#'
#' @return a names list.
#'
#' @export
#'
get_paper_size <- function(name, orientation = "ls") {

  ori <- c("ls","pt","ls","pt","ls","pt","ls","pt","ls","pt","ls")
  ori_inv <- c(ori[-1], ori[2])

  a_series <-
    data.frame(
      name = c("A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10"),
      width = as.integer(c( 841, 594, 420, 297, 210, 148, 105, 74, 52, 37, 26)),
      height = as.integer(c( 1189, 841, 594, 420, 297, 210, 148, 105, 74, 52, 37)),
      orientation = ori,
      units = rep("mm", times = length(ori))
    )

  a_series_inv <- a_series
  a_series_inv[["w"]] <- a_series_inv[["width"]]
  a_series_inv[["width"]] <- a_series_inv[["height"]]
  a_series_inv[["height"]] <- a_series_inv[["w"]]
  a_series_inv[["w"]] <- NULL
  a_series_inv[["orientation"]] <- ori_inv

  res <- rbind(a_series, a_series_inv)
  res <- res[order(res[["name"]]), ]

  return(as.list(
    res[res[["name"]] == name & res[["orientation"]] == orientation, ]
  ))

}
