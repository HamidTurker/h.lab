# Source
message("h.get.perievent_times:: v0.1: 2023 Feb 21")

# Function
h.get.perievent_times <- function(events, spikes, group = NULL,
                                  pre = 5, post = 5, max = 1000, allow_dupe = TRUE) {
  
  # Check args
  if (!allow_dupe) { message("Are you sure you want to remove counted spikes (thus, no duplicates)? That is often not recommended.") }
  
  # Initialize data frame
  n_events = length(events)
  if (is.null(group)) { # No groups
    df <- array(NA, c(max, n_events))
  } else { # With groups
    groups = unique(group)
    n_groups = length(groups)
    df <- array(NA, c(max, n_events, n_groups))
  }
  
  # Populate df with peri-event times surrounding each event
  if (is.null(group)) {
    
    for (i in 1:n_events) { # For each event..
      
      # Find this event window
      this_event = spikes[spikes >= (events[i]-pre) & spikes <= (events[i]+post)] # All marked time points within event i's window
      if (length(this_event) > 0) { df[1:length(this_event),i] = this_event }
      
    }
  } else {
    
    # Group info
    for (g in 1:n_groups) {
      group_id = groups[g]
      group_events = events[which(group == group_id)]
      n_group_events = length(group_events)
      
      # Group trial
      for (i in 1:n_group_events) { # For each event..
        
        # Find this event window
        this_event = spikes[spikes >= (group_events[i]-pre) & spikes <= (group_events[i]+post)] # All marked time points within event i's window
        if (length(this_event) > 0) { df[1:length(this_event),i] = this_event }
        
      }
    }
  }
  
  # Finished
  if (is.null(group)) {
    return(df)
  } else {
    return(list(df,groups))
  }
  
}

