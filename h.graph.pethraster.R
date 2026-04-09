# Source
message("h.graph.pethraster :: v0.1: 2026 April 6")

# Function
h.graph.pethraster <- function(events, spikes, group = NULL,
                               pre = 5, post = 5, bin = .1, alpha_val = .5,
                               allow_dupe = TRUE, other_events = NULL, 
                               peth_col_spike = NULL, 
                               rast_col_spike = NULL, rast_col_other_events = NULL,
                               plot = FALSE, plot_y = "count", theme = FALSE,
                               spike_size = 1, other_event_size = 5, sortby_eoi = 1,
                               per = NULL, x_lab = "Time (s)", event_marker_lwd = 2, group_line_size = 1,
                               font = NULL, plot_margins = c(2,1.7,0,.5)) {
  
  "
  Combined PETH and raster plot.
  "
  
  # Theme
  {
    plottheme <- theme(panel.background = element_rect(fill = "transparent", colour = NA),
                       plot.background = element_rect(fill = "transparent", colour = NA),
                       text = element_text(family = font),
                       axis.title.x = element_blank(), axis.title.y = element_blank(),
                       #axis.title.x   = element_text(family = "Helvetica Bold", size = 10),
                       #axis.title.y   = element_text(family = "Helvetica Bold", size = 10),
                       #axis.text.x   = element_text(family = "Helvetica Light", size = 5),
                       axis.ticks.length = unit(.1, "cm"),
                       axis.ticks.y = element_line(size = .5),
                       axis.ticks.x = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_text(family = "Helvetica Light", size = 12),
                       plot.title = element_text(size = 10, face = "bold")
    )
    line_int=0; line_col="black"; line_size=1; line_type="solid"
    #brk <- function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)
  }
  
  # Groups?
  if (is.null(group)) {
    
    # PETH
    plot.new()
    pushViewport(viewport(layout = grid.layout(2, 1)))
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    
    df <- h.graph.perievent(events=events, spikes=spikes,
                            pre=pre, post=post, bin=binsize,
                            other_events=other_events)
    
    p=ggplot(df[[1]], aes(x=time, y=Hz)) + plottheme +
      #scale_y_continuous(breaks = brk) +
      geom_area(stat='identity', position='identity', alpha=alpha_val, fill = peth_col_spike, col = "black", linewidth = .05) +
      geom_vline(xintercept=line_int, color=line_col, size=line_size, linetype=line_type) +
      theme(legend.position="none")
    print(p, vp = vplayout(1,1))
    
    # Raster
    pushViewport(vp=vplayout(2,1)); par(fig = gridFIG(), new = TRUE)
    par(mar=plot_margins) # 175 x 250 pixels    / 1.85 x 2.58 in   c(2,1.75,0,.5)
    
    h.graph.raster(events=events, spike_col=rast_col_spike,
                   pre=pre, post=post,
                   spikes=spikes,
                   other_events=other_events, spike_size=spike_size, other_event_size=other_event_size,
                   other_events_cols=rast_col_other_events, sortby_other_event=sortby_eoi,
                   per=per, xlab=x_lab, 
                   event_marker_lwd=event_marker_lwd, group_line_size=group_line_size, font=font)
    popViewport()
  }
  
  if (!is.null(group)) {
    
    # PETH
    plot.new()
    pushViewport(viewport(layout = grid.layout(2, 1)))
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    
    df <- h.graph.perievent(events=events, spikes=spikes, group=group,
                            pre=pre, post=post, bin=binsize,
                            other_events=other_events)
    
    p=ggplot(df[[1]], aes(x=time, y=Hz, fill = df[[1]]$group)) + plottheme +
      #scale_y_continuous(breaks = brk) +
      scale_fill_manual(values = peth_col_spike) + 
      geom_area(stat='identity', position='identity', alpha=alpha_val, col = "black", linewidth = .05) +
      geom_vline(xintercept=line_int, color=line_col, size=line_size, linetype=line_type) +
      theme(legend.position="none")
    print(p, vp = vplayout(1,1))
    
    # Raster
    pushViewport(vp=vplayout(2,1)); par(fig = gridFIG(), new = TRUE)
    par(mar=plot_margins) # 175 x 250 pixels    / 1.85 x 2.58 in   c(2,1.75,0,.5)
    
    h.graph.raster(events=events, spike_col=rast_col_spike,
                   pre=pre, post=post,
                   spikes=spikes, group=group, group_colors=rast_col_spike,
                   other_events=other_events, spike_size=spike_size, other_event_size=other_event_size,
                   other_events_cols=rast_col_other_events, sortby_other_event=sortby_eoi,
                   per=per, xlab=x_lab, 
                   event_marker_lwd=event_marker_lwd, group_line_size=group_line_size, font=font)
    popViewport()
    
  }
  
  
}
