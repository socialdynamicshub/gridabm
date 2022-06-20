#' Get the Moore neighborhood of a specified cell
#'
#' @param i Row index.
#' @param j Column index.
#' @param dims Vector containing board dimensions (rows, columns).
#' @param periodic Whether to employ periodic boundary conditions or not.
#'
#' @return A list of coordinates of Moore neighborhood cells.
#' @export
#'
#' @examples
#' get_moore_neighborhood(5, 5, c(20, 20))
get_moore_neighborhood <- function(i, j, dims, periodic = TRUE) {

  if (periodic) {
    if (i == 1) {
      t <- dims[1]
    } else {
      t <- i - 1
    }
    if (i == dims[1]) {
      b <- 1
    } else {
      b <- i + 1
    }
    if (j == 1) {
      l <- dims[2]
    } else {
      l <- j - 1
    }
    if (j == dims[2]) {
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
      if (!(0 %in% p) && (p[1] != (dims[1] + 1)) && (p[2] != (dims[2] + 1))) {
        positions <- append(positions, list(p))
      }
    }

  }

  return(positions)
}
