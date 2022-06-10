library(dplyr)

#' Animate a model run
#'
#' @param df The simulation data.
#' @param marker_size Size of the markers.
#' @param color_scheme Which colors to use for which cell state.
#'
#' @return A gganimate animation
#' @export
#'
#' @examples
animate_model_run <- function(df, marker_size, color_scheme) {
  anim <- df %>%
    plot_state(marker_size, color_scheme) +
    gganimate::transition_states(step)

  return(anim)
}
