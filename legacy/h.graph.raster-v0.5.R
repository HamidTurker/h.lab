h.graph.raster <- function(events, spikes, per = NULL, group = NULL,
                           pre = 5, post = 5, bin = .001, flip_per = FALSE,
                           plot = FALSE, group_colors = NULL, event_marker_col = NULL,
                           ylab = NULL, xlab = NULL, other_events = NULL, other_events_cols = NULL) {
  
  # v0.5: 2023 Jan 26
  
  events=trials$arriv; spikes=na.omit(cell.array[,1,i]); pre=pre_window; post=post_window; per=trials$trial; xlab="Time (s)"; ylab="Trial"
  
  # Initialize histogram data frame
  raster_frame <- array(0, dim=c(length(unique(per)), length(seq(-pre,post,bin))))
  colnames(raster_frame) <- as.character(seq(-pre,post,bin))
  per <- 1:length(per); rownames(raster_frame) <- as.character(unique(per))
  n_events = length(events)
  

  # Populate the raster data frame
  for (i in 1:n_events) { # For each event..
    
    # Which per-output does this event belong to?
    per_entry = per[i]
    
    # Find this trial section (i.e., window surrounding event based on pre and post)
    start = events[i]-pre # Time point for start of pre-window
    end = events[i]+post+bin # Time point for end of post-window (make sure to include last bin)
    this_trial = spikes[spikes >= start & spikes <= end] # All spikes within trial i's window surrounding the event
    
    # If there are spikes on this trial..
    if (!length(this_trial)==0) {
      
      # Spike times in relation to start of this trial
      time_corrected_spikes=this_trial-start
      
      # Modulate out bin size and correct time for pre-window size. This gives us the bin entries where a spike occurred.
      bin_entries=time_corrected_spikes-time_corrected_spikes%%bin-pre
      
      # Mark the bin_entries in the raster_frame
      raster_frame[per_entry,as.character(unique(bin_entries))]=1
    }
  }
  
  
  # Any other events to be marked?
  if (!is.null(other_events)) {
    n_other_marks = length(other_events)
    other_event_times = array(NA, c(n_events,n_other_marks))
    
    for (i in 1:n_other_marks) {
      other_event_times[,i] = as.vector(unlist(trials[other_events[i]])) - events
    }
    
  }
  
  # Plot raster
  if (is.null(group)) { # No groups
    
    # Flip per-axis?
    if (flip_per) {
      plot(x="Time (s)",y="Trial",ylim=rev(c(.5,n_events+1)),xlim=c(-pre,post))
      if (!is.null(event_marker_col)) {abline(v=0, col=event_marker_col)}
      for (i in 1:n_events) {
        clip(x1=-pre, x2=post, y1=(i-.5), y2=(i+.5))
        #abline(h=i, lwd=1/10, col="lightgrey")
        abline(v=bin*which(raster_frame[i,]==1)-pre)
      } 
    } else {
      plot(x=NULL,xlab=xlab,ylab=ylab,ylim=c(.5,n_events+1),xlim=c(-pre,post))
      
      if (!is.null(event_marker_col)) {abline(v=0, col=event_marker_col)}
      
      # Mark raster for each reference event
      for (i in 1:n_events) {
        
        clip(x1=-pre, x2=post, y1=(i-.5), y2=(i+.5))
        #abline(h=i, lwd=1/10, col="lightgrey")
        abline(v=bin*which(raster_frame[i,]==1)-pre, col=mark_col)
        
        # Also mark other events, if requested
        if (!is.null(other_event_times)) {
          for (j in 1:n_other_events) {
            other_event_mark_size=5
            abline(v=other_event_times[i,j], col=other_events_cols[j], lwd=other_event_mark_size)
          }
        }
      } 
    }
    
  } else { # Groups
    
    # Convert to data frame, add grouping variable, order, and rename
    raster_frame = as.data.frame(raster_frame)
    raster_frame$group = group
    
    if (flip_per) { 
      raster_frame = raster_frame[order(raster_frame$group, decreasing = TRUE), ]
      group_labels <- sort(unique(group), decreasing = TRUE)
    } else {
      raster_frame = raster_frame[order(raster_frame$group), ]
      group_labels <- sort(unique(group))
    }
    
    plot(x=NULL,xlab=xlab,ylab=ylab,ylim=c(.5,n_events+1),xlim=c(-pre,post),tck=-.03)
    abline(col="black",lwd=2,h=which(c(FALSE, !as.character(raster_frame$group)[-1] == as.character(raster_frame$group)[-length(raster_frame$group)]))-.5)
    if (!is.null(event_marker_col)) {abline(v=0, col=event_marker_col)}
    for (i in 1:length(group)) {
      clip(x1=-pre, x2=post, y1=(i-.5), y2=(i+.5))
      #abline(h=i, lwd=1/10, col="lightgrey")
      for (j in 1:length(group_labels)) {
        if (group_labels[j]==raster_frame$group[i]) {
          abline(v=bin*which(raster_frame[i,]==1)-pre, col=group_colors[j])
        }
      }
    }
  }
}

