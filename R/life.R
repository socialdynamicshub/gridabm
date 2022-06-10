library(dplyr)

life_step <- function(m) {
  axis_size <- dim(m)[1]
  m_upd <- matrix(nrow = axis_size, ncol = axis_size, rep(0, axis_size^2))

  for (equator in 1:axis_size) {
    for (meridian in 1:axis_size) {
      positions <- get_moore_neighborhood(equator, meridian, axis_size, periodic = TRUE)
      neigh_sum <- 0

      for (pos in positions) {
        neigh_sum <- neigh_sum + m[pos[1], pos[2]]
      }

      if (m[equator, meridian] == 1) {
        if (neigh_sum %in% c(2, 3)) {
          m_upd[equator, meridian] <- 1
        }
      } else {
        if (neigh_sum == 3) {
          m_upd[equator, meridian] <- 1
        }
      }

    }
  }

  return(m_upd)
}

life_game <- function(initial_state, steps) {

  m <- initial_state
  d <- board_to_df(m)
  d$step <- 0
  d$cell_id <- 1:nrow(d)
  for (i in 1:steps) {
    m_upd <- life_step(m)
    d_step <- board_to_df(m_upd)
    d_step$step <- i
    d_step$cell_id <- 1:nrow(d_step)
    d <- dplyr::bind_rows(d, d_step)
    m <- m_upd
  }
  d <- dplyr::select(d, step, cell_id, row, col, state)
  d$row <- as.numeric(d$row)
  d$col <- as.numeric(d$col)
  d$cell_id <- as.factor(d$cell_id)
  d$state <- as.factor(d$state)

  return(d)
}
