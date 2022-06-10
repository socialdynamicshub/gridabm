library(ggplot2)

grid_plot <- function(grid_df, marker_size, color_scheme) {
  
  # TODO: check if data is suitable
  
  axis_size <- max(grid_df$row)
   
  p <- grid_df %>%
    ggplot(aes(x = col, y = row, fill = state, group = cell_id)) +
    # ggplot(aes(x = col, y = row, fill = state)) +  # TODO: add id in life
    geom_point(size = marker_size, shape = 22, color = "transparent") +
    scale_fill_manual(values = color_scheme) +
    scale_x_continuous(
      minor_breaks = seq(0.5, axis_size + 0.5, 1),
      breaks = seq(0, axis_size)
    ) +
    scale_y_continuous(
      trans = "reverse",
      minor_breaks = seq(0.5, axis_size + 0.5, 1),
      breaks = seq(0, axis_size)
    ) +
    # transition_states(step, transition_length = 1, state_length = 1) +
    coord_fixed() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.background = element_rect(fill = "transparent") ,
      panel.grid.minor = element_line(color = "lightgrey"),
      panel.grid.major = element_line(color = "transparent"),
      panel.border = element_rect(fill = "transparent", color = "darkgrey", size = 3)
    )

  return(p)
}