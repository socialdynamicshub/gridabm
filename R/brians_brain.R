#' One step in Brian's brain
#'
#' @param m The current state.
#'
#' @return A matrix representing the subsequent state
#' @export
#'
#' @examples
brians_brain_step <- function(m) {
  axis_size <- dim(m)[1]
  m_upd <- matrix(nrow = axis_size, ncol = axis_size, rep(0, axis_size^2))

  for (equator in 1:axis_size) {
    for (meridian in 1:axis_size) {

      if (m[equator, meridian] == 2) {
        m_upd[equator, meridian] <- 0
      } else if (m[equator, meridian] == 1) {
        m_upd[equator, meridian] <- 2
      } else {
        positions <- get_moore_neighborhood(equator, meridian, axis_size, periodic = TRUE)
        alive_neighbor_count <- 0
        for (pos in positions) {
          if (m[pos[1], pos[2]] == 1) {
            alive_neighbor_count <- alive_neighbor_count + 1
          }
        }
        if (alive_neighbor_count == 2) {
          m_upd[equator, meridian] <- 1
        }
      }

    }
  }

  return(m_upd)
}

# brians_brain_game <- function(initial_state, steps) {
#
#   m <- initial_state
#   d <- board_to_df(m)
#   d$step <- 0
#   for (i in 1:steps) {
#     m <- brians_brain_step(m)
#     d_step <- board_to_df(m)
#     d_step$step <- i
#     d <- dplyr::bind_rows(d, d_step)
#   }
#   d <- dplyr::select(d, step, cell_id, row, col, state)
#
#   return(d)
# }
