#' Run an automaton with a given stepping function
#'
#' @param initial_state The initial state of the automaton.
#' @param steps The number of steps to run the automaton.
#' @param stepfunc The stepping function.
#' @param ... Additional parameters for the stepping function.
#'
#' @return A dataframe with the results of the simulation
#' @export
#'
#' @examples
run_automaton <- function(initial_state, steps, stepfunc, ...) {

  m <- initial_state
  d <- board_to_df(m)
  d$step <- 0
  for (i in 1:steps) {
    m <- stepfunc(m, ...)
    d_step <- board_to_df(m)
    d_step$step <- i
    d <- dplyr::bind_rows(d, d_step)
  }
  d <- dplyr::select(d, step, cell_id, row, col, state)

  return(d)
}
