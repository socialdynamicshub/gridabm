#' Run an automaton with a given stepping function
#'
#' Automata can be updated with stepping functions one step at a time.
#' However, the intended workflow of this package is to give a stepping function
#' to the `run_automaton` function as an input parameter.
#' This function takes the `stepfunc` and successively applies it `steps` times,
#' starting from an `initial_state`.
#' Some stepping functions require additional input that can be provided to this
#' function as additional parameters (`...`).
#' Several well-known automata are implemented in this package (see `life_step`,
#' `schelling_step`, `forest_fire_step`, `brians_brain_step`).
#'
#' @param initial_state The initial state `matrix` of the automaton.
#' @param steps The number of steps to run the automaton.
#' @param stepfunc The stepping function.
#' @param ... Additional parameters for the stepping function.
#'
#' @return A `data.frame` with the results of the simulation.
#' @export
#'
#' @examples
#' initial_state <- create_board(2, c(0.7, 0.3), c(20, 20))
#' run_automaton(initial_state, 50, life_step)
run_automaton <- function(initial_state, steps, stepfunc, ...) {

  board <- initial_state
  d <- board_to_df(board)
  d$step <- 0
  for (i in 1:steps) {
    board <- stepfunc(board, ...)
    d_step <- board_to_df(board)
    d_step$step <- i
    d <- dplyr::bind_rows(d, d_step)
  }
  d <- dplyr::select(d, step, cell_id, row, col, state)

  return(d)
}
