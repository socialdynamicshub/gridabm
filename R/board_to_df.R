#' Turn an automaton state `matrix` into a `data.frame`
#'
#' Automaton states are represented as `matrix` objects in the `gridabm`
#' library.
#' However, `data.frame`s are more easy to plot, so this function converts a
#' `matrix` into a `data.frame`.
#'
#' @param m A `matrix` representing an automaton state.
#'
#' @return The `data.frame` representation of the automaton state.
#' @export
#'
#' @examples
#' automaton_state <- matrix(
#'   sample(c(0, 1), replace = TRUE, prob = c(0.5, 0.5), 100),
#'   nrow = 10, ncol = 10
#' )
#'
#' board_to_df(automaton_state)
board_to_df <- function(m) {
  warn <- getOption("warn")
  options(warn = -1)  # switch off warnings temporarily

  # axis_size <- dim(m)[1]

  d <- data.frame(m)
  names(d) <- c(seq(1, dim(m)[2]))

  d <- d %>%
    dplyr::bind_cols(
      data.frame(
        row = seq(1, dim(m)[1], 1)
      )
    ) %>%
    tidyr::pivot_longer(
      d,
      cols = seq(1, dim(m)[2]),
      names_to = "col",
      values_to = "state"
    )  %>%
    dplyr::mutate(
      row = as.numeric(row),
      col = as.numeric(col)
    ) %>%
    dplyr::arrange(row, col) %>%
    dplyr::mutate(
      cell_id = as.factor(1:nrow(.)),
      state = as.factor(state),
    ) %>%
    dplyr::select(cell_id, row, col, state)

  options(warn = warn)  # switch warnings back on

  return(d)
}
