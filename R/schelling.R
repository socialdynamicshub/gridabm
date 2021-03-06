#' Find a random free position in the Schelling game
#'
#' @param board The current state.
#'
#' @return Coordinates of a random free position
#' @export
#'
#' @examples
#' state <- create_board(3, c(0.1, 0.45, 0.45), c(20, 20))
#' plot_state(state, 5, theme_schelling())
#' random_free_pos(state)
random_free_pos <- function(board) {
  if (!(0 %in% board)) {
    print("There are no free positions.")
    return(NULL)
  }
  pos_x <- sample(1:dim(board)[1], 1)
  pos_y <- sample(1:dim(board)[2], 1)
  pos <- c(pos_x, pos_y)
  if (board[pos[1], pos[2]] == 0) {
    return(pos)
  } else {
    pos <- random_free_pos(board)
  }
  return(pos)
}

#' Compute the happiness of each agent in a Schelling run
#'
#' @param board The current state.
#' @param tolerance The tolerance level.
#'
#' @return A matrix with corresponding dimensions indicating agent happiness
#' @export
#'
#' @examples
#' state <- create_board(3, c(0.1, 0.45, 0.45), c(20, 20))
#' plot_state(state, 5, theme_schelling())
#' get_happiness_mat(state, 3)
get_happiness_mat <- function(board, tolerance) {
  happiness <- matrix(1, nrow = dim(board)[1], ncol = dim(board)[2])

  for (i in 1:dim(board)[1]) {
    for (j in 1:dim(board)[2]) {
      agent_type <- board[i, j]
      if (agent_type == 0) {
        next
      }
      positions <- get_moore_neighborhood(i, j, dim(board), periodic = FALSE)
      diff_count <- 0
      for (pos in positions) {
        if (board[pos[1], pos[2]] != agent_type) {
          diff_count <- diff_count + 1
        }
      }
      if (diff_count > tolerance) {
        happiness[i, j] <- 0
      }
    }
  }

  return(happiness)
}

#' Step the Schelling model
#'
#' @param board The current state.
#' @param tolerance The tolerance level.
#'
#' @return The updated state
#' @export
#'
#' @examples
#' initial_state <- create_board(3, c(0.1, 0.45, 0.45), c(20, 20))
#'
#' # Visualize initial state
#' plot_state(initial_state, 5, theme_schelling())
#'
#' next_state <- schelling_step(initial_state, tolerance = 3)
#'
#' # Visualize subsequent state
#' plot_state(next_state, 5, theme_schelling())
schelling_step <- function(board, tolerance) {
  happiness <- get_happiness_mat(board, tolerance)
  board_upd <- board
  for (i in 1:dim(board)[1]) {
    for (j in 1:dim(board)[2]) {
      if ((happiness[i, j] == 0)) {
        new_pos <- random_free_pos(board_upd)
        board_upd[new_pos[1], new_pos[2]] <- board[i, j]
        board_upd[i, j] <- 0
      }
    }
  }

  return(board_upd)
}
