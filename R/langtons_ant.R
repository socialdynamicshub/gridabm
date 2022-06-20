#' Create a grid for Langton's Ant
#'
#' @param antpos A vector giving the coordinates of the ant on the grid (row,
#'   column).
#' @param antorient One of "north", "east", "south", and "west". Defaults to
#'   "north" if orientation is invalid.
#' @param axis_size The size of the grid dimensions.
#' @param state_dist Distribution of states as given to `create_grid`.
#'
#' @return A `matrix` suitable as initial state for Langton's Ant.
#' @export
#'
#' @examples
#' initial_state <- create_ant_grid(c(15, 15), "north", 30)
#' plot_state(initial_state, 3, theme_langtons_ant())
create_ant_grid <- function(
    antpos, dims,
    antorient = "north",
    state_dist = c(1, 0)
) {
  m <- create_grid(2, state_dist, dims)
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


#' One step in Langton's Ant
#'
#' Langton's Ant is a cellular automaton where an "ant" moves across the grid
#' leaving behind black or white cells according to a specific rule.
#' For more information, check out
#' <https://en.wikipedia.org/wiki/Langton%27s_ant>.
#'
#' @param m A `matrix` representing the current state.
#'
#' @return A `matrix` representing the next state according to Langton's Ant.
#' @export
#'
#' @examples
langtons_ant_step <- function(m) {
  axis_size <- dim(m)[1]
  m_upd <- m

  antpos <- which(m > 1, arr.ind = TRUE)
  antval <- m[antpos[1], antpos[2]]
  northpos <- get_direct_neighbor(antpos[1], antpos[2], dim(m), "north")
  eastpos <- get_direct_neighbor(antpos[1], antpos[2], dim(m), "east")
  southpos <- get_direct_neighbor(antpos[1], antpos[2], dim(m), "south")
  westpos <- get_direct_neighbor(antpos[1], antpos[2], dim(m), "west")

  # cell colors: white => 0, black => 1

  # (white, north) => 2
  if (antval == 2) {
    m_upd[antpos[1], antpos[2]] <- 1
    if (m[eastpos[1], eastpos[2]] == 0) {
      m_upd[eastpos[1], eastpos[2]] <- 3
    } else {
      m_upd[eastpos[1], eastpos[2]] <- 7
    }

  # (white, east) => 3
  } else if (antval == 3) {
    m_upd[antpos[1], antpos[2]] <- 1
    if (m[southpos[1], southpos[2]] == 0) {
      m_upd[southpos[1], southpos[2]] <- 4
    } else {
      m_upd[southpos[1], southpos[2]] <- 8
    }

  # (white, south) => 4
  } else if (antval == 4) {
    m_upd[antpos[1], antpos[2]] <- 1
    if (m[westpos[1], westpos[2]] == 0) {
      m_upd[westpos[1], westpos[2]] <- 5
    } else {
      m_upd[westpos[1], westpos[2]] <- 9
    }

  # (white, west) => 5
  } else if (antval == 5) {
    m_upd[antpos[1], antpos[2]] <- 1
    if (m[northpos[1], northpos[2]] == 0) {
      m_upd[northpos[1], northpos[2]] <- 2
    } else {
      m_upd[northpos[1], northpos[2]] <- 6
    }

  # (black, north) => 6
  } else if (antval == 6) {
    m_upd[antpos[1], antpos[2]] <- 0
    if (m[westpos[1], westpos[2]] == 0) {
      m_upd[westpos[1], westpos[2]] <- 5
    } else {
      m_upd[westpos[1], westpos[2]] <- 9
    }

  # (black, east) => 7
  } else if (antval == 7) {
    m_upd[antpos[1], antpos[2]] <- 0
    if (m[northpos[1], northpos[2]] == 0) {
      m_upd[northpos[1], northpos[2]] <- 2
    } else {
      m_upd[northpos[1], northpos[2]] <- 6
    }

  # (black, south) => 8
  } else if (antval == 8) {
    m_upd[antpos[1], antpos[2]] <- 0
    if (m[eastpos[1], eastpos[2]] == 0) {
      m_upd[eastpos[1], eastpos[2]] <- 3
    } else {
      m_upd[eastpos[1], eastpos[2]] <- 7
    }

  # (black, west) => 9
  } else if (antval == 9) {
    m_upd[antpos[1], antpos[2]] <- 0
    if (m[southpos[1], southpos[2]] == 0) {
      m_upd[southpos[1], southpos[2]] <- 4
    } else {
      m_upd[southpos[1], southpos[2]] <- 8
    }
  }

  return(m_upd)
}
