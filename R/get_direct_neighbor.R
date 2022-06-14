#' Get neighbor position at given orientation
#'
#' @param i Row index.
#' @param j Column index.
#' @param axis_size Dimension of the grid in each direction.
#' @param orientation One of "north", "east", "south", and "west".
#'
#' @return The position of the neighbor.
#' @export
#'
#' @examples
#' get_direct_neighbor(10, 10, 20, "north")
get_direct_neighbor <- function(i, j, axis_size, orientation) {
  if (orientation == "north") {
    if (i == 1) {
      return(c(axis_size, j))
    } else {
      return(c(i - 1, j))
    }
  } else if (orientation == "east") {
    if (j == axis_size) {
      return(c(i, 1))
    } else {
      return(c(i, j + 1))
    }
  } else if (orientation == "south") {
    if (i == axis_size) {
      return(c(1, j))
    } else {
      return(c(i + 1, j))
    }
  } else if (orientation == "west") {
    if (j == 1) {
      return(c(i, axis_size))
    } else {
      return(c(i, j - 1))
    }
  } else {
    warning("Invalid orientation, defaulted to current cell position.")
    return(c(i, j))
  }
}
