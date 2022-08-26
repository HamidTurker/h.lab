h.calc.firingrate_for_event <- function(events, spikes, pre = 0, post = 1, group = NULL) {
  
  # v0.2: 2022 August 25
  
  # Calculate the firing rate surrounding a (list of) events
  # Provide the time point(s) at which the event(s) occurred, the time points at
  # which spikes occurred, the time window surrounding the event (pre, post, in seconds).
  # Optionally, provide a grouping variable for the events. This will take all events,
  # and average the firing rate within each group.
  
  # The script will return a 1 (no group provided) or 2-column array (group provided)
  # The length of the array will be the length of the number of events (no group provided) or number of groups (group provided)
  
  ######################################################################################################
  ######################################################################################################
  
  # Initialize firing rate data frame
  if (is.null(group)) {
    firing_frame <- data.frame(
      events = events,
      count_spk = rep(0,length(events)),
      Hz = 0
    )
  } else {
    firing_frame <- data.frame(
      group = sort(unique(group)),
      count_grp = 0,
      count_spk = 0,
      Hz = 0
    )
  }
  
  # Populate the firing rate data frame
  if (is.null(group)) {
    for (i in 1:length(events)) { # For each event.. 
      
      # Find this event section (i.e., window surrounding event based on pre and post)
      start = events[i]-pre # Time point for start of pre-window
      end = events[i]+post # Time point for end of post-window
      this_event = spikes[spikes >= start & spikes <= end] # All spikes within event i's window surrounding the event
      
      # Add to spike counter
      firing_frame$count_spk[i] <- length(this_event)
      
    }
    
    # Calculate Hz
    windowsize <- abs(post-pre)
    firing_frame$Hz <- firing_frame$count_spk / windowsize
    
    # Return
    return(firing_frame[,c("events","Hz")])
    
  } else {
    for (i in 1:length(events)) { # For each event..
      
      # Which group?
      group_entry <- group[i]
      
      # Add to group counter
      firing_frame$count_grp[firing_frame$group == group_entry] <- firing_frame$count_grp[firing_frame$group == group_entry] + 1
      
      # Find this event section (i.e., window surrounding event based on pre and post)
      start = events[i]-pre # Time point for start of pre-window
      end = events[i]+post # Time point for end of post-window
      this_event = spikes[spikes >= start & spikes <= end] # All spikes within event i's window surrounding the event
      
      # Add to spike counter
      firing_frame$count_spk[firing_frame$group == group_entry] <- firing_frame$count_spk[firing_frame$group == group_entry] + length(this_event)
      
    }
    
    # Calculate Hz
    windowsize <- abs(post-pre)
    firing_frame$Hz <- firing_frame$count_spk / (firing_frame$count_grp * windowsize)
    
    # Return
    return(firing_frame[,c("group","Hz")])
  }
}