#' Get neighbor position at given orientation
#'
#' @param i Row index.
#' @param j Column index.
#' @param dims Vector containing board dimensions (rows, columns).
#' @param orientation One of "north", "east", "south", and "west".
#'
#' @return The position of the neighbor.
#' @export
#'
#' @examples
#' get_direct_neighbor(10, 10, c(20, 20), "north")
get_direct_neighbor <- function(i, j, dims, orientation) {
  if (orientation == "north") {
    if (i == 1) {
      return(c(dims[1], j))
    } else {
      return(c(i - 1, j))
    }
  } else if (orientation == "east") {
    if (j == dims[2]) {
      return(c(i, 1))
    } else {
      return(c(i, j + 1))
    }
  } else if (orientation == "south") {
    if (i == dims[1]) {
      return(c(1, j))
    } else {
      return(c(i + 1, j))
    }
  } else if (orientation == "west") {
    if (j == 1) {
      return(c(i, dims[2]))
    } else {
      return(c(i, j - 1))
    }
  } else {
    warning("Invalid orientation, defaulted to current cell position.")
    return(c(i, j))
  }
}
