#' Plot a cellular automaton state
#'
#' All visualizations in the `gridabm` library are `ggplot2` objects and can be
#' customized accordingly.
#' This function either takes a state `matrix` or a `data.frame` as returned by
#' `board_to_df` and returns a visualization of the state as a `ggplot2` object.
#' Marker size and color scheme can be customized as well.
#' The default color palette used is based on the `Dark2` palette from
#' `RColorBrewer`.
#'
#' @param state A `data.frame` or a `matrix` representing the current state.
#' @param marker_size How big the markers on the plot should be.
#' @param color_scheme What colors to use for each cell state.
#'
#' @return A `ggplot2` object of the current state.
#' @export
#'
#' @examples
#' state <- create_board(4, c(0.1, 0.3, 0.3, 0.3), c(20, 20))
#' plot_state(state)
plot_state <- function(state, marker_size = 5, color_scheme = theme_default()) {

  if (is.matrix(state)) {
    state <- board_to_df(state)
  }

  n_rows <- max(state$row)
  n_cols <- max(state$col)

  p <- state %>%
    ggplot2::ggplot(
      ggplot2::aes(x = col, y = row, fill = state, group = cell_id)
    ) +
    ggplot2::geom_point(size = marker_size, shape = 22, color = "transparent") +
    ggplot2::scale_fill_manual(values = color_scheme) +
    ggplot2::scale_x_continuous(
      minor_breaks = seq(0.5, n_cols + 0.5, 1),
      breaks = seq(-1, n_cols + 1),
      limits = c(0.5, n_cols + 0.5)
    ) +
    ggplot2::scale_y_continuous(
      trans = "reverse",
      minor_breaks = seq(n_rows + 1.5, -0.5, -1),
      breaks = seq(-1, n_rows + 1),
      limits = c(n_rows + 0.5, 0.5)
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
