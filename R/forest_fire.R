#' Convenience function to create an initial state for the forest fire model
#'
#' The forest fire model is often initialized as a grid where the left
#' most column is "on fire".
#' This function returns an initial state of this kind.
#' The forest size and the tree density can be provided as well.
#'
#' @param dims Vector containing board dimensions (rows, columns).
#' @param tree_density The density of the forest given as a probability for the
#'   R base `sample` function.
#'
#' @return A matrix that represents a forest on fire.
#' @export
#'
#' @examples
#' forest <- create_forest_board(c(20, 20), 0.6)
#' plot_state(forest, 5, theme_forest_fire())
create_forest_board <- function(dims, tree_density) {
  board <- create_board(2, c(1 - tree_density, tree_density), dims)
  # board <- matrix(
  #   sample(
  #     c(0, 1),
  #     prob = c(1 - tree_density, tree_density),
  #     replace = TRUE,
  #     size = prod(dims)
  #   ),
  #   nrow = dims[1],
  #   ncol = dims[2]
  # )
  board[, 1] <- board[, 1] * 2  # set forest on fire!
  return(board)
}

#' Run the forest fire model one step
#'
#' This function takes in a forest state and outputs the next state after
#' applying the forest fire update rule.
#' Check out <https://en.wikipedia.org/wiki/Forest-fire_model> for more
#' information.
#'
#' @param board A `matrix` representing a forest.
#'
#' @return The updated forest.
#' @export
#'
#' @examples
#' forest <- create_forest_board(c(20, 20), 0.6)
#'
#' # Visualize the initial state
#' plot_state(forest, 5, theme_forest_fire())
#'
#' next_state <- forest_fire_step(forest)
#'
#' # Visualize the subsequent state
#' plot_state(next_state, 5, theme_forest_fire())
forest_fire_step <- function(board) {

  axis_size <- dim(board)[1]
  board_upd <- board

  for (equator in 1:dim(board)[1]) {
    for (meridian in 1:dim(board)[2]) {
      if (board[equator, meridian] == 2) {
        positions <- get_von_neumann_neighborhood(
          equator, meridian, dim(board), periodic = FALSE
        )
        for (pos in positions) {
          if (board[pos[1], pos[2]] == 1) {
            board_upd[pos[1], pos[2]] <- 2
          }
        }
      }
    }
  }

  for (i in 1:dim(board)[1]) {
    for (j in 1:dim(board)[2]) {
      if (board[i, j] == 2) {
        board_upd[i, j] <- 3
      }
    }
  }

  return(board_upd)
}
