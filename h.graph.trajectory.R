h.graph.trajectory <- function(trajectory, spikes,
                               path_size, dot_size, dot_color,
                               title = NULL) {
  
  # Function to make trajectory plots with my personal preferred ggplot aesthetics
  
  ggplot(trajectory, aes(trajectory$x_pos,trajectory$y_pos)) +
    geom_path(size=path_size) +
    geom_point(data=spikes, colour=dot_color, size=dot_size) +
    ggtitle(title) +
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      axis.text.x=element_blank(), axis.text.y=element_blank(),
      axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
      axis.title.x=element_blank(), axis.title.y=element_blank())
  
}