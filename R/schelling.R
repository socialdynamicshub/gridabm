library(dplyr)

#'
#' @param board The current state.
#'
#' @return A position on the board
#' @export
#'
#' @examples
random_free_pos <- function(board) {
  if (!(0 %in% board)) {
    print("There are no free positions.")
    return(NULL)
  }
  axis_size <- dim(board)[1]
  pos <- sample(1:axis_size, replace = TRUE, 2)
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
get_happiness_mat <- function(board, tolerance) {
  axis_size <- dim(board)[1]
  happiness <- matrix(1, nrow = axis_size, ncol = axis_size)

  for (i in 1:axis_size) {
    for (j in 1:axis_size) {
      agent_type <- board[i, j]
      if (agent_type == 0) {
        next
      }
      positions <- get_moore_neighborhood(i, j, axis_size, periodic = FALSE)
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
schelling_step <- function(board, tolerance) {
  happiness <- get_happiness_mat(board, tolerance)
  board_upd <- board
  for (i in 1:20) {
    for (j in 1:20) {
      if ((happiness[i, j] == 0)) {
        new_pos <- random_free_pos(board_upd)
        board_upd[new_pos[1], new_pos[2]] <- board[i, j]
        board_upd[i, j] <- 0
      }
    }
  }

  return(board_upd)
}

#' Run the Schelling model for a specified number of steps
#'
#' @param initial_state The initial state of the board.
#' @param tolerance The tolerance level.
#' @param steps How many steps to run the model.
#'
#' @return A dataframe with the simulation results.
#' @export
#'
#' @examples
schelling_game <- function(initial_state, tolerance, steps) {
  board <- initial_state
  d <- board_to_df(board)
  d$step <- 0
  for (i in 1:steps) {
    board <- schelling_step(board, tolerance)
    d_step <- board_to_df(board)
    d_step$step <- i
    d <- dplyr::bind_rows(d, d_step)
  }
  d <- dplyr::select(d, step, cell_id, row, col, state)

  return(d)
}
