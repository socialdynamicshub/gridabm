library(dplyr)

#' Create a forest to burn
#'
#' @param axis_size Axis size of the forest.
#' @param tree_density How many trees to put on the board relatively.
#'
#' @return A matrix that represents a forest.
#' @export
#'
#' @examples
create_forest <- function(axis_size, tree_density) {
  forest <- matrix(
    sample(
      c(0, 1), prob = c(1 - tree_density, tree_density),
      replace = TRUE, size = axis_size^2),
    ncol = axis_size, nrow = axis_size
  )
  forest[, 1] <- 2  # set forest on fire!
  return(forest)
}

#' Run the forest model one step
#'
#' @param forest A matrix representing a forest
#'
#' @return The updated forest
#' @export
#'
#' @examples
forest_fire_step <- function(forest) {
  axis_size <- dim(forest)[1]
  forest_upd <- forest

  for (equator in 1:axis_size) {
    for (meridian in 1:axis_size) {
      if (forest[equator, meridian] == 2) {
        positions <- get_von_neumann_neighborhood(
          equator, meridian, axis_size, periodic = FALSE
        )
        for (pos in positions) {
          if (forest[pos[1], pos[2]] == 1) {
            forest_upd[pos[1], pos[2]] <- 2
          }
        }
      }
    }
  }

  for (i in 1:axis_size) {
    for (j in 1:axis_size) {
      if (forest[i, j] == 2) {
        forest_upd[i, j] <- 3
      }
    }
  }

  return(forest_upd)
}

#' Run the forest fire for a specified number of steps
#'
#' @param initial_state The initial forest.
#' @param steps How many steps to run the model for.
#'
#' @return A dataframe with the tabular representation of the forest fire
#' @export
#'
#' @examples
forest_fire_game <- function(initial_state, steps) {

  forest <- initial_state
  d <- board_to_df(forest)
  d$step <- 0
  for (i in 1:steps) {
    forest <- forest_fire_step(forest)
    d_step <- board_to_df(forest)
    d_step$step <- i
    d <- dplyr::bind_rows(d, d_step)
  }
  d <- dplyr::select(d, step, cell_id, row, col, state)

  return(d)
}
