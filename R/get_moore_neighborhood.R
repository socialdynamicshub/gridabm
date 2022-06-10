get_moore_neighborhood <- function(i, j, axis_size, periodic = TRUE) {
  
  if (periodic) {
    if (i == 1) {
      t <- axis_size
    } else {
      t <- i - 1
    }
    if (i == axis_size) {
      b <- 1
    } else {
      b <- i + 1
    }
    if (j == 1) {
      l <- axis_size
    } else {
      l <- j - 1
    }
    if (j == axis_size) {
      r <- 1
    } else {
      r <- j + 1
    }
    
    positions <- list(
      c(t, l),
      c(t, j),
      c(t, r),
      c(i, l),
      c(i , r),
      c(b, l),
      c(b, j),
      c(b, r)
    )
  } else {
    t <- i - 1
    b <- i + 1
    l <- j - 1
    r <- j + 1
    
    positions_tmp <- list(
      c(t, l),
      c(t, j),
      c(t, r),
      c(i, l),
      c(i , r),
      c(b, l),
      c(b, j),
      c(b, r)
    )
    
    positions <- list()
    
    for (p in positions_tmp) {
      if (!(0 %in% p) && !((axis_size + 1) %in% p)) {
        positions <- append(positions, list(p))
      }
    }
    
  }
  
  return(positions)
}
