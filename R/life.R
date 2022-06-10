#' One step in Conway's life
#'
#' @param m The current state.
#'
#' @return A matrix representing the subsequent state
#' @export
#'
#' @examples
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

#' Run Conway's life for a specified number of steps
#'
#' @param initial_state The initial state.
#' @param steps How many steps to run the model for.
#'
#' @return A dataframe with the simulation results
#' @export
#'
#' @examples
life_game <- function(initial_state, steps) {

  m <- initial_state
  d <- board_to_df(m)
  d$step <- 0
  for (i in 1:steps) {
    m <- life_step(m)
    d_step <- board_to_df(m)
    d_step$step <- i
    d <- dplyr::bind_rows(d, d_step)
  }
  d <- dplyr::select(d, step, cell_id, row, col, state)

  return(d)
}
