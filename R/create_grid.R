#' Create a state grid
#'
#' This function returns a `matrix` with `n_states` distinct cell states.
#' The frequency distribution of cell states can be supplied using the
#' parameter `state_dist`.
#' Finally, `axis_size` controls the size of the grid.
#'
#' @param n_states The number of distinct cell states to use for the grid.
#' @param state_dist The distributaion of cell states.
#' @param axis_size The grid dimension in each direction.
#'
#' @return A `matrix` representing an automaton state.
#' @export
#'
#' @examples
#' create_grid(5, rep(0.2, 5), 20)
create_grid <- function(n_states, state_dist, axis_size) {
  grid <- matrix(
    sample(
      seq(0, n_states - 1),
      replace = TRUE,
      prob = state_dist,
      axis_size^2
    ),
    nrow = axis_size, ncol = axis_size
  )
  return(grid)
}
