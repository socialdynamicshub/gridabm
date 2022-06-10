library(dplyr)

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

move_agents <- function(board, happiness) {
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

schelling_game <- function(initial_state, tolerance, steps) {

  board <- initial_state
  d <- board_to_df(board)
  d$step <- 0
  d$cell_id <- 1:nrow(d)
  for (i in 1:steps) {
    happiness <- get_happiness_mat(board, tolerance)
    board <- move_agents(board, happiness)
    d_step <- board_to_df(board)
    d_step$step <- i
    d_step$cell_id <- 1:nrow(d_step)
    d <- dplyr::bind_rows(d, d_step)
  }
  d <- dplyr::select(d, step, cell_id, row, col, state)
  d$row <- as.numeric(d$row)
  d$col <- as.numeric(d$col)
  d$cell_id <- as.factor(d$cell_id)
  d$state <- as.factor(d$state)

  return(d)
}

schelling_game_multi <- function(initial_state, param_levels, steps) {
  d <- data.frame()

  for (tol in param_levels) {
    board <- initial_state
    d_tol <- board_to_df(board)
    d_tol$step <- 0
    d_tol$cell_id <- seq(1, length(board))
    for (i in 1:steps) {
      happiness <- get_happiness_mat(board, tol)
      board <- move_agents(board, happiness)
      d_tol_step <- board_to_df(board)
      d_tol_step$step <- i
      d_tol_step$cell_id <- seq(1, length(board))
      d_tol <- dplyr::bind_rows(d_tol, d_tol_step)
    }
    d_tol$row <- as.numeric(d_tol$row)
    d_tol$col <- as.numeric(d_tol$col)
    d_tol$cell_id <- as.factor(d_tol$cell_id)
    d_tol$state <- as.factor(d_tol$state)
    d_tol$tolerance <- tol
    d_tol$tolerance <- as.numeric(d_tol$tolerance)
    d_tol <- dplyr::select(d_tol, tolerance, step, cell_id, row, col, state)
    d <- dplyr::bind_rows(d, d_tol)
  }

  return(d)
}
