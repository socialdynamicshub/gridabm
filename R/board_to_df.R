#' Turn a board matrix into a dataframe
#'
#' @param m A matrix
#'
#' @return A dataframe
#' @export
#'
#' @examples
board_to_df <- function(m) {
  warn <- getOption("warn")
  options(warn = -1)  # switch off warnings temporarily

  axis_size <- dim(m)[1]

  d <- data.frame(m)
  names(d) <- c(seq(1, axis_size))

  d <- d %>%
    dplyr::bind_cols(
      data.frame(
        row = seq(axis_size, 1, -1)
      )
    ) %>%
    tidyr::pivot_longer(
      d,
      cols = seq(1, axis_size),
      names_to = "col",
      values_to = "state"
    ) %>%
    dplyr::mutate(
      cell_id = as.factor(1:nrow(.)),
      row = as.numeric(row),
      col = as.numeric(col),
      state = as.factor(state),
    ) %>%
    dplyr::select(cell_id, row, col, state)

  options(warn = warn)  # switch warnings back on

  return(d)
}
