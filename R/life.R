#' One step in Conway's Life
#'
#' Conway's Life is perhaps the most famous cellular automaton.
#' This function evolves a given state by one step applying the life update
#' rule.
#' The input `matrix` is expected to have the states 0 and 1.
#'
#' For more information, check out
#' <https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life>.
#'
#' @param m The current state.
#'
#' @return A matrix representing the subsequent state
#' @export
#'
#' @examples
#' initial_state <- matrix(
#'   sample(c(0, 1), prob = c(0.7, 0.3), replace = TRUE, 400),
#'   nrow = 20, ncol = 20
#' )
#'
#' # Visualize initial state
#' plot_state(initial_state, 5, theme_life())
#'
#' next_state <- life_step(initial_state)
#'
#' # Visualize subsequent state
#' plot_state(next_state, 5, theme_life())
life_step <- function(m) {
  axis_size <- dim(m)[1]
  m_upd <- matrix(nrow = axis_size, ncol = axis_size, rep(0, axis_size^2))

  for (equator in 1:axis_size) {
    for (meridian in 1:axis_size) {
      positions <- get_moore_neighborhood(equator, meridian, axis_size, periodic = TRUE)
      neigh_sum <- 0

      for (pos in positions) {
        neigh_sum <- neigh_sum + m[pos[1], pos[2]]
      }

      if (m[equator, meridian] == 1) {
        if (neigh_sum %in% c(2, 3)) {
          m_upd[equator, meridian] <- 1
        }
      } else {
        if (neigh_sum == 3) {
          m_upd[equator, meridian] <- 1
        }
      }

    }
  }

  return(m_upd)
}
