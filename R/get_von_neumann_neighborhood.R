#' Get Von Neumann neighborhood of a specified cell
#'
#' @param i Row index.
#' @param j Column index.
#' @param dims Vector containing board dimensions (rows, columns).
#' @param periodic Whether to employ periodic boundary conditions or not.
#'
#' @return A list of coordinates of Von Neumann neighborhood cells.
#' @export
#'
#' @examples
#' get_von_neumann_neighborhood(5, 5, 20)
get_von_neumann_neighborhood <- function(i, j, dims, periodic = TRUE) {

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
      c(t, j),
      c(i, l),
      c(i , r),
      c(b, j)
    )

  } else {
    t <- i - 1
    b <- i + 1
    l <- j - 1
    r <- j + 1

    positions_tmp <- list(
      c(t, j),
      c(i, l),
      c(i , r),
      c(b, j)
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
