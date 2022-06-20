#' Create a board for Langton's Ant
#'
#' @param antpos A vector giving the coordinates of the ant on the grid (row,
#'   column).
#' @param dims Vector containing board dimensions (rows, columns).
#' @param antorient One of "north", "east", "south", and "west". Defaults to
#'   "north" if orientation is invalid.
#' @param state_dist Distribution of states as given to `create_board`.
#'
#' @return A `matrix` suitable as initial state for Langton's Ant.
#' @export
#'
#' @examples
#' initial_state <- create_ant_board(c(15, 15), "north", 30)
#' plot_state(initial_state, 3, theme_langtons_ant())
create_ant_board <- function(
    antpos, dims,
    antorient = "north",
    state_dist = c(1, 0)
) {
  m <- create_board(2, state_dist, dims)
  if (antorient == "north") {
    m[antpos[1], antpos[2]] <- 2
  } else if (antorient == "east") {
    m[antpos[1], antpos[2]] <- 3
  } else if (antorient == "south") {
    m[antpos[1], antpos[2]] <- 4
  } else if (antorient == "west") {
    m[antpos[1], antpos[2]] <- 5
  } else {
    warning("Invalid ant orientation. Defaulting to \"north\"")
    m[antpos[1], antpos[2]] <- 2
  }
  return(m)
}


#' One step in the Langton's Ant automaton
#'
#' Langton's Ant is a cellular automaton where an "ant" moves across the grid
#' leaving behind black or white cells according to a specific rule.
#' For more information, check out
#' <https://en.wikipedia.org/wiki/Langton%27s_ant>.
#'
#' @param board A `matrix` representing the current state.
#'
#' @return A `matrix` representing the next state according to Langton's Ant.
#' @export
#'
#' @examples
#' initial_state <- create_ant_board(c(10, 10), c(20, 20), "north")
#'
#' # Visualize initial state
#' plot_state(initial_state, 5, theme_langtons_ant())
#'
#' next_state <- langtons_ant_step(initial_state)
#'
#' # Visualize next state
#' plot_state(next_state, 5, theme_langtons_ant())
langtons_ant_step <- function(board) {
  axis_size <- dim(board)[1]
  board_upd <- board

  antpos <- which(board > 1, arr.ind = TRUE)
  antval <- board[antpos[1], antpos[2]]
  northpos <- get_direct_neighbor(antpos[1], antpos[2], dim(board), "north")
  eastpos <- get_direct_neighbor(antpos[1], antpos[2], dim(board), "east")
  southpos <- get_direct_neighbor(antpos[1], antpos[2], dim(board), "south")
  westpos <- get_direct_neighbor(antpos[1], antpos[2], dim(board), "west")

  # cell colors: white => 0, black => 1

  # (white, north) => 2
  if (antval == 2) {
    board_upd[antpos[1], antpos[2]] <- 1
    if (board[eastpos[1], eastpos[2]] == 0) {
      board_upd[eastpos[1], eastpos[2]] <- 3
    } else {
      board_upd[eastpos[1], eastpos[2]] <- 7
    }

  # (white, east) => 3
  } else if (antval == 3) {
    board_upd[antpos[1], antpos[2]] <- 1
    if (board[southpos[1], southpos[2]] == 0) {
      board_upd[southpos[1], southpos[2]] <- 4
    } else {
      board_upd[southpos[1], southpos[2]] <- 8
    }

  # (white, south) => 4
  } else if (antval == 4) {
    board_upd[antpos[1], antpos[2]] <- 1
    if (board[westpos[1], westpos[2]] == 0) {
      board_upd[westpos[1], westpos[2]] <- 5
    } else {
      board_upd[westpos[1], westpos[2]] <- 9
    }

  # (white, west) => 5
  } else if (antval == 5) {
    board_upd[antpos[1], antpos[2]] <- 1
    if (board[northpos[1], northpos[2]] == 0) {
      board_upd[northpos[1], northpos[2]] <- 2
    } else {
      board_upd[northpos[1], northpos[2]] <- 6
    }

  # (black, north) => 6
  } else if (antval == 6) {
    board_upd[antpos[1], antpos[2]] <- 0
    if (board[westpos[1], westpos[2]] == 0) {
      board_upd[westpos[1], westpos[2]] <- 5
    } else {
      board_upd[westpos[1], westpos[2]] <- 9
    }

  # (black, east) => 7
  } else if (antval == 7) {
    board_upd[antpos[1], antpos[2]] <- 0
    if (board[northpos[1], northpos[2]] == 0) {
      board_upd[northpos[1], northpos[2]] <- 2
    } else {
      board_upd[northpos[1], northpos[2]] <- 6
    }

  # (black, south) => 8
  } else if (antval == 8) {
    board_upd[antpos[1], antpos[2]] <- 0
    if (board[eastpos[1], eastpos[2]] == 0) {
      board_upd[eastpos[1], eastpos[2]] <- 3
    } else {
      board_upd[eastpos[1], eastpos[2]] <- 7
    }

  # (black, west) => 9
  } else if (antval == 9) {
    board_upd[antpos[1], antpos[2]] <- 0
    if (board[southpos[1], southpos[2]] == 0) {
      board_upd[southpos[1], southpos[2]] <- 4
    } else {
      board_upd[southpos[1], southpos[2]] <- 8
    }
  }

  return(board_upd)
}
