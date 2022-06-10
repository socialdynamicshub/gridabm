library(dplyr)

create_forest <- function(axis_size, tree_density) {
  forest <- matrix(
    sample(
      c(0, 1), prob = c(1 - tree_density, tree_density),
      replace = TRUE, size = axis_size^2),
    ncol = axis_size, nrow = axis_size
  )
  forest[, 1] <- 2
  return(forest)
}

forest_fire_step <- function(forest) {
  axis_size <- dim(forest)[1]
  forest_upd <- forest

  for (equator in 1:axis_size) {
    for (meridian in 1:axis_size) {
      if (forest[equator, meridian] == 2) {
        positions <- get_von_neumann_neighborhood(equator, meridian, axis_size, periodic = FALSE)
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

forest_fire_game <- function(initial_state, steps) {
  forest <- initial_state
  d <- board_to_df(forest)
  d$step <- 0
  d$cell_id <- 1:nrow(d)
  for (i in 1:steps) {
    forest_upd <- forest_fire_step(forest)
    d_step <- board_to_df(forest_upd)
    d_step$step <- i
    d_step$cell_id <- 1:nrow(d_step)
    d <- bind_rows(d, d_step)
    forest <- forest_upd
  }
  d <- select(d, step, cell_id, row, col, state)
  d$row <- as.numeric(d$row)
  d$col <- as.numeric(d$col)
  d$cell_id <- as.factor(d$cell_id)
  d$state <- as.factor(d$state)
  return(d)
}
