#' Plot a grid ABM state
#'
#' @param state A dataframe or a matrix representing the current state.
#' @param marker_size How big the markers on the plot should be.
#' @param color_scheme What colors to use for each cell state.
#'
#' @return A ggplot2 object of the current state.
#' @export
#'
#' @examples
plot_state <- function(state, marker_size, color_scheme) {

  if (is.matrix(state)) {
    state <- board_to_df(state)
  }

  axis_size <- max(state$row)

  p <- state %>%
    ggplot2::ggplot(
      ggplot2::aes(x = col, y = row, fill = state, group = cell_id)
    ) +
    # ggplot(aes(x = col, y = row, fill = state)) +  # TODO: add id in life
    ggplot2::geom_point(size = marker_size, shape = 22, color = "transparent") +
    ggplot2::scale_fill_manual(values = color_scheme) +
    ggplot2::scale_x_continuous(
      minor_breaks = seq(0.5, axis_size + 0.5, 1),
      breaks = seq(0, axis_size)
    ) +
    ggplot2::scale_y_continuous(
      trans = "reverse",
      minor_breaks = seq(0.5, axis_size + 0.5, 1),
      breaks = seq(0, axis_size)
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = "transparent") ,
      panel.grid.minor = ggplot2::element_line(color = "lightgrey"),
      panel.grid.major = ggplot2::element_line(color = "transparent"),
      panel.border = ggplot2::element_rect(fill = "transparent", color = "darkgrey", size = 3)
    )

  return(p)
}
