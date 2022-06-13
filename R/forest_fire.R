#' Convenience function to create an initial state for the forest fire model
#'
#' The forest fire model is often initialized as a square grid where the left
#' most column is "on fire".
#' This function returns an initial state of this kind.
#' The forest size and the tree density can be provided as well.
#'
#' @param axis_size Axis size of the forest. In the `gridabm` library, all
#'   automata run on square grids, so both dimensions are the same.
#' @param tree_density The density of the forest given as a probability for the
#'   R base `sample` function.
#'
#' @return A matrix that represents a forest on fire.
#' @export
#'
#' @examples
#' forest <- create_forest(20, 0.6)
#' plot_state(forest, 5, theme_forest_fire())
create_forest <- function(axis_size, tree_density) {
  forest <- matrix(
    sample(
      c(0, 1),
      prob = c(1 - tree_density, tree_density),
      replace = TRUE,
      size = axis_size^2
    ),
    ncol = axis_size,
    nrow = axis_size
  )
  forest[, 1] <- 2  # set forest on fire!
  return(forest)
}

#' Run the forest fire model one step
#'
#' This function takes in a forest state and outputs the next state after
#' applying the forest fire update rule.
#' Check out <https://en.wikipedia.org/wiki/Forest-fire_model> for more
#' information.
#'
#' @param forest A `matrix` representing a forest.
#'
#' @return The updated forest.
#' @export
#'
#' @examples
#' forest <- create_forest(20, 0.6)
#'
#' # Visualize the initial state
#' plot_state(forest, 5, theme_forest_fire())
#'
#' next_state <- forest_fire_step(forest)
#'
#' # Visualize the subsequent state
#' plot_state(next_state, 5, theme_forest_fire())
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
