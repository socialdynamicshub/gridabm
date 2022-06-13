#' One step in the Brian's Brain automaton
#'
#' Brian's Brain is an automaton with three distinct states that produces some
#' characteristic chaotic patterns.
#' The function is usually given as the `stepfunc` argument to `run_automaton`.
#' Check out <https://en.wikipedia.org/wiki/Brian's_Brain> for more information.
#'
#' @param m The current state of the automaton. This matrix should have three
#'   distinct states: 0, 1, and 2.
#'
#' @return A matrix representing the subsequent state.
#' @export
#'
#' @examples
#' initial_state <- matrix(
#'   sample(c(0, 1, 2), replace = TRUE, prob = c(0.5, 0.25, 0.25), 200),
#'   nrow = 20, ncol = 20
#' )
#'
#' # Visualization of the initial state
#' plot_state(initial_state, 5, theme_brians_brain_light())
#'
#' next_state <- brians_brain_step(initial_state)
#'
#' # Visualization of the subsequent state
#' plot_state(next_state, 5, theme_brians_brain_light())
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
        positions <- get_moore_neighborhood(
          equator,
          meridian,
          axis_size,
          periodic = TRUE
        )
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
