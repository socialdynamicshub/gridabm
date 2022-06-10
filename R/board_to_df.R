#' Turn a board matrix into a dataframe
#'
#' @param m A matrix
#'
#' @return A dataframe
#' @export
#'
#' @examples
board_to_df <- function(m) {

  axis_size <- dim(m)[1]
  d <- data.frame(m)
  names(d) <- seq(1, axis_size)
  d <- tidyr::pivot_longer(
    d,
    cols = names(d),
    names_to = "col",
    values_to = "state"
  )
  d$row <- rep(seq(1, axis_size), each = axis_size)
  d <- dplyr::select(d, row, col, state)
  d$row <- as.numeric(d$row)
  d$col <- as.numeric(d$col)
  d$state <- as.factor(d$state)

  return(d)
}
