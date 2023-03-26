# Source
message("h.graph.raster :: v0.6: 2023 March 25")

# Function
h.graph.raster2 <- function(events, spikes, per = NULL, flip_per = FALSE, pre = 5, post = 5, bin = .001, no_plot = FALSE, 
                            group = NULL, group_colors = NULL, event_marker_col = "black", event_marker_lwd = 1, spike_size = 1, spike_col = "grey",
                            group_line_size = 2, group_line_col = "black", ylab = NULL, xlab = NULL, other_events = NULL, other_events_cols = NULL, sortby_other_event = NULL,
                            other_event_size = 5, raster_lines = FALSE, raster_line_size = 1/5, raster_line_col = "lightgrey") {
  
  "
  Create a raster plot from the provided spike train, event/stimulus times, and 
  a time window around those events. Additionally, you can split the raster up
  based on a grouping variable as well as provide other event times you want marked.
  
  
    Args:
      events (vector)         : Time stamps of critical events or stimuli that you want
                                to plot the raster of spikes in reference to (i.e., your 
                                time=0 on the x-axis).
      spikes (vector)         : Time stamps of when a spike was recorded.
      per (vector)            : Plot the raster 'per' what (on the y-axis)? Typically,
                                there is one row 'per' trial, in which case you should provide
                                a vector of trial ID numbers of each event time.
      flip_per (bool)         : Flip the y-axis (TRUE) or not (FALSE)?
      pre (num)               : Window prior to event to be plotted (in seconds; 
                                i.e. if pre = 5 or pre = -5, the function will plot spikes in the 5 seconds
                                leading up to the event of interest.
      post (num)              : Window following the event to be plotted (in seconds).
      bin (num)               : Time bin for the raster.
      no_plot (bool)          : If TRUE, don't make a plot, just return the created raster (as data frame).
      group (vector)          : Grouping variable of each event time.
      group_colors (char)     : Color for each group's raster (e.g., group_colors = c('red','green')).
      event_marker_col (char) : Color of the x=0 marker on the x-axis.
      event_marker_lwd (num)  : Linewidth of the x=0 marker on the x-axis.
      spike_size (num)        : Size of the spike markings on the raster.
      spike_col (char)        : Color of the spike markings on the raster (if there are no groups).
      group_line_size (num)   : Size of the line that marks splits between groups on the raster.
      group_line_col (char)   : Color of the line that marks splits between groups on the raster.
      ylab (char)             : Label on the y-axis
      xlab (char)             : Label on the x-axis
      other_events (list)     : List of vectors of other event times to be marked (events which
                                are not the event being used as reference point at x=0). Each
                                vector in the list must have length equal to length(events) and ordered
                                to match the ordering of the events vector. If a given trial does not
                                have a certain other_event to be marked, leave that entry as NA.
      other_events_cols (char): Colors to mark the other events in (e.g., other_events_cols=c('red','green','blue'),
                                for 3 other events to be marked).
      sortby_other_event (num): If, additionally, you want trials to be sorted by one of your other_events,
                                provide the index of that other_event in the other_events list (e.g., if you have
                                an other_events list with 3 vectors and you want your raster to be sorted by the
                                second vector in that last, pass sortby_other_event = 2).
      other_event_size (num)  : Size of the other_event markers.                        
      raster_lines (bool)     : Add guiding raster lines to plot.
      raster_line_size (num)  : Size of the guiding raster lines.
      raster_line_col (char)  : Color of the guiding raster lines.
      
                                
    Returns:
      Peri-event spike raster as either a figure or a data frame.
  "
  
  # Check args
  {
    if (pre < 0 | post < 0 ) { pre = abs(pre); post = abs(post) }
    
    if (max(events)>max(spikes)) { warning("There are event times occurring later than the last spike time. Is that correct?") }
    if (min(events)<min(spikes)) { warning("There are event times occurring earlier than the first spike time. Is that correct?") }
    
    if (!is.null(group)) {
      if (!length(group)==length(events)) { stop("Each provided event does not have a corresponding group label.") }
      if (is.null(group_colors)) { stop("You didn't assign group colors.") }
      if (!length(group_colors)==length(unique(group))) { stop("The number of group_colors doesn't match the number of unique group labels.") }
    }
    
    if (!is.null(other_events)) {
      if (length(other_events)==1) { other_events = list(other_events) }
      if (!class(other_events)=="list") { stop("Your other_events must be in a list of vectors.") }
      for (i in 1:length(other_events)) {
        if (!length(other_events[[i]])==length(events)) { 
          stop("Each provided other_event does not have a corresponding event. There has to be the same number of elements in each other_event vector as the main event vector.") }
      }
      if (is.null(other_events_cols)) { stop("You didn't assign other_events_cols.") }
      if (!length(other_events_cols)==length(other_events)) { stop("The number of other_events_cols doesn't match the number of other_event vectors.") }
      if (sortby_other_event > length(other_events)) { stop("The sortby_other_event index is larger than the number of other_event vectors.") }
    }
  }
  
  # Initialize
  raster_frame <- array(0, dim=c(length(unique(per)), length(seq(-pre,post,bin))))
  colnames(raster_frame) <- as.character(seq(-pre,post,bin))
  per <- 1:length(per); rownames(raster_frame) <- as.character(unique(per))
  n_events = length(events)
  if (!is.null(group)) { n_groups = length(unique(group)) }
  
  # Mark spikes in the raster surrounding each event
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
  
  # Any other_events to be marked? How many other_events do we have?
  if (!is.null(other_events)) {
    n_other_events = length(other_events)
    other_event_times = array(NA, c(n_events,n_other_events))
  
    # Adjust other_event times in relation to the main event marker
    for (i in 1:n_other_events) {
      other_event_times[,i] = as.numeric(unlist(other_events[i])) - events
    }
  }
  
  ########## Plot raster
  if (no_plot) { # Make a plot or just return raster_frame?
    
    return(as.data.frame(raster_frame)) # No plot, just return the data frame
    
  } else { # Make a plot
    
    # Are we sorting the graph based on other events?
    if (is.null(sortby_other_event)) { # Don't sort by one of the other_events
      
      # Do we have groups?
      if (is.null(group)) { # Don't sort by other_events, don't sort by groups
        {
          # Flip per-axis?
          if (flip_per) { # Don't sort by other_events, don't sort by groups, do flip y-axis
            
            # Set up raster
            plot(x=NULL,xlab=xlab,ylab=ylab,ylim=rev(c(.5,n_events+1)),xlim=c(-pre,post))
            if (!is.null(event_marker_col)) {abline(v=0, col=event_marker_col, lwd=event_marker_lwd)}
            
            # Mark raster for each spike
            for (i in 1:n_events) {
              clip(x1=-pre, x2=post, y1=(i-.5), y2=(i+.5))
              if (raster_lines) {abline(h=i, lwd=raster_line_size, col=raster_line_col)}
              abline(v=bin*which(raster_frame[i,]==1)-pre, lwd=spike_size, col=spike_col)
              
              # Also mark other events, if requested
              if (!is.null(other_event_times)) {
                for (j in 1:n_other_events) {
                  abline(v=other_event_times[i,j], col=other_events_cols[j], lwd=other_event_mark_size)
                }
              }
            } 
          } else { # Don't sort by other_events, don't sort by groups, don't flip y-axis
            
            # Set up raster
            plot(x=NULL,xlab=xlab,ylab=ylab,ylim=c(.5,n_events+1),xlim=c(-pre,post))
            if (!is.null(event_marker_col)) {abline(v=0, col=event_marker_col, lwd=event_marker_lwd)}
            
            # Mark raster for each spike
            for (i in 1:n_events) {
              clip(x1=-pre, x2=post, y1=(i-.5), y2=(i+.5))
              if (raster_lines) {abline(h=i, lwd=raster_line_size, col=raster_line_col)}
              abline(v=bin*which(raster_frame[i,]==1)-pre, lwd=spike_size, col=spike_col)
              
              # Also mark other events, if requested
              if (!is.null(other_event_times)) {
                for (j in 1:n_other_events) {
                  abline(v=other_event_times[i,j], col=other_events_cols[j], lwd=other_event_size)
                }
              }
            } 
          }
        }
        
      } else { # Don't sort by other_events, but do sort by groups
        {      
          # Convert to data frame, add grouping variable, order, and rename
          raster_frame = as.data.frame(raster_frame)
          raster_frame$group = group
          
          # Order the raster
          if (flip_per) {
            ordering <- order(raster_frame$group, decreasing = TRUE) # Match the group IDs to trial IDs
            raster_frame <- raster_frame[ordering,] # Reorder trials to group them based on group IDs
            group_labels <- sort(unique(group), decreasing = TRUE) # Also reorder the group_labels..
            group_colors <- rev(group_colors) # ..and group colors
            if (!is.null(other_event_times)) { other_event_times = other_event_times[ordering,] } # If there are other_events, remember to reorder those too.
          } else {
            ordering <- order(raster_frame$group) # Match the group IDs to trial IDs
            raster_frame <- raster_frame[ordering,] # Reorder trials to group them based on group IDs
            group_labels <- sort(unique(group)) # Also reorder the group_labels. Group colors are already in the right order.
            if (!is.null(other_event_times)) { other_event_times = other_event_times[ordering,] } # If there are other_events, remember to reorder those too.
          }
          
          # Set up raster
          plot(x=NULL,xlab=xlab,ylab=ylab,ylim=c(.5,n_events+1),xlim=c(-pre,post),tck=-.02)
          abline(col=group_line_col,lwd=group_line_size,h=which(c(FALSE, !as.character(raster_frame$group)[-1] == as.character(raster_frame$group)[-length(raster_frame$group)]))-.5)
          if (!is.null(event_marker_col)) {abline(v=0, col=event_marker_col, lwd=event_marker_lwd)}
          
          # Mark raster for each spike
          for (i in 1:n_events) {
            clip(x1=-pre, x2=post, y1=(i-.5), y2=(i+.5))
            if (raster_lines) {abline(h=i, lwd=raster_line_size, col=raster_line_col)}
            for (j in 1:length(group_labels)) {
              if (group_labels[j]==raster_frame$group[i]) {
                abline(v=bin*which(raster_frame[i,]==1)-pre, col=group_colors[j])
              }
            }
            
            # Also mark other events, if requested
            if (!is.null(other_event_times)) {
              for (k in 1:n_other_events) {
                abline(v=other_event_times[i,k], col=other_events_cols[k], lwd=other_event_size)
              }
            }
          }
        }
        
      }

    } else { # Sort by one of the other_events
      
      # Do we have groups?
      if (is.null(group)) { # Sort by other_events, don't sort by groups
        {
          # Reorder based on chosen other_event
          ordering <- order(other_event_times[,sortby_other_event])
          other_event_times <- other_event_times[ordering,]
          raster_frame <- raster_frame[ordering,]
          
          # Flip per-axis?
          if (flip_per) { # Sort by other_events, don't sort by groups, do flip y-axis
            plot(x=NULL,xlab=xlab,ylab=ylab,ylim=rev(c(.5,n_events+1)),xlim=c(-pre,post))
          } else { # Don't sort by other_events, don't sort by groups, don't flip y-axis
            plot(x=NULL,xlab=xlab,ylab=ylab,ylim=c(.5,n_events+1),xlim=c(-pre,post))
          }
          
          # Set up raster
          if (!is.null(event_marker_col)) {abline(v=0, col=event_marker_col, lwd=event_marker_lwd)}
          
          # Mark raster for each spike in the peri-event window
          for (i in 1:n_events) {
            clip(x1=-pre, x2=post, y1=(i-.5), y2=(i+.5))
            if (raster_lines) {abline(h=i, lwd=raster_line_size, col=raster_line_col)}
            abline(v=bin*which(raster_frame[i,]==1)-pre, lwd=spike_size, col=spike_col)
            
            # And mark the other events in that peri-event window
            for (j in 1:n_other_events) {
              abline(v=other_event_times[i,j], col=other_events_cols[j], lwd=other_event_mark_size)
            }
          }
          
        }
        
      } else { # Sort by other_events, also sort by groups
        {      
          # Convert to data frame and add grouping variable
          raster_frame = as.data.frame(raster_frame)
          raster_frame$group = group
          
          # Order the raster
          if (flip_per) {
            
            # First, order the raster based on the groups
            ordering <- order(raster_frame$group, decreasing = TRUE)
            raster_frame <- raster_frame[ordering,]
            group_labels <- sort(unique(group), decreasing = TRUE)
            group_colors <- rev(group_colors)
            other_event_times = other_event_times[ordering,]
            
            # Next, reorder again based on the other_event_time within each group
            for (i in 1:n_groups) {
              group_trials <- which(raster_frame$group==group_labels[i]) # Which trials belong to this group?
              hold <- other_event_times[group_trials,] # Put those trials aside
              ordering <- order(hold[,sortby_other_event]) # Find their new ordering
              hold <- hold[ordering,] # Implement the new ordering
              other_event_times[group_trials,] <- hold # Place this group's reordered trials back in main array
              
              hold <- raster_frame[group_trials,] # Remember to reorder the spike data itself too
              hold <- hold[ordering,] # Implement the new ordering
              raster_frame[group_trials,] <- hold # Place them back
            }
            
            } else {
              
              # First, order the raster based on the groups
              ordering <- order(raster_frame$group)
              raster_frame <- raster_frame[ordering,]
              group_labels <- sort(unique(group))
              other_event_times = other_event_times[ordering,]
              
              # Next, reorder again based on the other_event_time within each group
              for (i in 1:n_groups) {
                group_trials <- which(raster_frame$group==group_labels[i]) # Which trials belong to this group?
                hold <- other_event_times[group_trials,] # Put those trials aside
                ordering <- order(hold[,sortby_other_event]) # Find their new ordering
                hold <- hold[ordering,] # Implement the new ordering
                other_event_times[group_trials,] <- hold # Place this group's reordered trials back in main array
                
                hold <- raster_frame[group_trials,] # Remember to reorder the spike data itself too
                hold <- hold[ordering,] # Implement the new ordering
                raster_frame[group_trials,] <- hold # Place them back
              }
          }
          
          # Set up raster
          plot(x=NULL,xlab=xlab,ylab=ylab,ylim=c(.5,n_events+1),xlim=c(-pre,post),tck=-.02)
          abline(col=group_line_col,lwd=group_line_size,h=which(c(FALSE, !as.character(raster_frame$group)[-1] == as.character(raster_frame$group)[-length(raster_frame$group)]))-.5)
          if (!is.null(event_marker_col)) {abline(v=0, col=event_marker_col, lwd=event_marker_lwd)}
          
          # Mark raster for each spike
          for (i in 1:n_events) {
            clip(x1=-pre, x2=post, y1=(i-.5), y2=(i+.5))
            if (raster_lines) {abline(h=i, lwd=raster_line_size, col=raster_line_col)}
            for (j in 1:length(group_labels)) {
              if (group_labels[j]==raster_frame$group[i]) {
                abline(v=bin*which(raster_frame[i,]==1)-pre, col=group_colors[j])
              }
            }
            
            # And mark the other events in that peri-event window
            for (k in 1:n_other_events) {
              abline(v=other_event_times[i,k], col=other_events_cols[k], lwd=other_event_size)
            }
          }
        }
        
      }
      
    }
  }
  
}
