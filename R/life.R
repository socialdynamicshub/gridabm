#' One step in the Conway's Life automaton
#'
#' Conway's Life is perhaps the most famous cellular automaton.
#' This function evolves a given state by one step applying the life update
#' rule.
#' The input `matrix` is expected to have the states 0 and 1.
#'
#' For more information, check out
#' <https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life>.
#'
#' @param board The current state.
#'
#' @return A matrix representing the subsequent state
#' @export
#'
#' @examples
#' initial_state <- create_board(2, c(0.7, 0.3), c(20, 20))
#'
#' # Visualize initial state
#' plot_state(initial_state, 5, theme_life())
#'
#' next_state <- life_step(initial_state)
#'
#' # Visualize subsequent state
#' plot_state(next_state, 5, theme_life())
life_step <- function(board) {
  board_upd <- matrix(0, nrow = dim(board)[1], ncol = dim(board)[2])

  for (equator in 1:dim(board)[1]) {
    for (meridian in 1:dim(board)[2]) {
      positions <- get_moore_neighborhood(
        equator, meridian,
        dim(board),
        periodic = TRUE
      )
      neigh_sum <- 0

      for (pos in positions) {
        neigh_sum <- neigh_sum + board[pos[1], pos[2]]
      }

      if (board[equator, meridian] == 1) {
        if (neigh_sum %in% c(2, 3)) {
          board_upd[equator, meridian] <- 1
        }
      } else {
        if (neigh_sum == 3) {
          board_upd[equator, meridian] <- 1
        }
      }

    }
  }

  return(board_upd)
}
