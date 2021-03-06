#' Animate a model run
#'
#' `animate_model_run` takes the results of an automaton run and animates it
#' leveraging the `gganimate` library.
#' You can specify the marker size on the grid and customize the color scheme.
#'
#' @param df The simulation data as returned by the `run_automaton` function.
#' @param marker_size Size of the cells on the grid
#'   (needs adjustment depending on grid size).
#' @param color_scheme Which colors to use for which cell state. As the cell
#'   states are defined by a sequence of integers starting at 0, you can simply
#'   specify the colors by providing a vector with each entry `i` representing
#'   the color for state `i`. To specify colors for specific states, provide a
#'   named vector, where the names are the cell states in backticks (e.g.,
#'   c(`0` = "transparent", `1` = "black")).
#'
#' @return A gganimate animation.
#' @export
#'
#' @examples
#' initial_state <- create_board(4, rep(0.25, 4), c(20, 20))
#'
#' results <- run_automaton(initial_state, 100, schelling_step, tolerance = 3)
#'
#' animate_model_run(
#'   results,
#'   5,
#'   c(`0` = "transparent", `1` = "gold2", `2` = "royalblue", `3` = "coral")
#' )
animate_model_run <- function(df, marker_size, color_scheme) {
  anim <- df %>%
    plot_state(marker_size, color_scheme) +
    gganimate::transition_states(step)

  return(anim)
}
