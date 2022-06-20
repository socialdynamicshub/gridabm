#' Create a board
#'
#' This function returns a `matrix` with `n_states` distinct cell states.
#' The frequency distribution of cell states can be supplied using the
#' parameter `state_dist`.
#' Finally, `axis_size` controls the size of the grid.
#'
#' @param n_states The number of distinct cell states to use for the grid.
#' @param state_dist The distribution of cell states.
#' @param dims Vector containing board dimensions (rows, columns).
#'
#' @return A `matrix` representing an automaton state.
#' @export
#'
#' @examples
#' create_grid(5, rep(0.2, 5), c(20, 20))
create_board <- function(n_states, state_dist, dims) {
  grid <- matrix(
    sample(
      seq(0, n_states - 1),
      replace = TRUE,
      prob = state_dist,
      prod(dims)
    ),
    nrow = dims[1], ncol = dims[2]
  )
  return(grid)
}
