h.graph.perievent_ribbon <- function(events, spikes, group = NULL,
                                     pre = 5, post = 5, bin = .1,
                                     allow_dupe = TRUE) {
  
  # v0.2: 2023 feb 14
  
  # Make a peri-event ribbon plot around events (events, in seconds into global time),
  # with a pre-event window (pre, in seconds, default = 5 s),
  # and post-event window (post, in seconds, default = 5 s),
  # with a given bin size (bin, in seconds, default = 0.1 s),
  # for time points where spikes occurred (spikes, in seconds into global time).
  # If there are trial conditions, provide them as an ordered list (group),
  # where index matches the index for the ordered events (i.e. group[i] is for events[i])
  
  # Check args
  if (!allow_dupe) { message("Are you sure you want to remove counted spikes (thus, no duplicates)? That is often not recommended.") }
  
  # Initialize histogram data frame
  if (is.null(group)) { # No groups
    histogram_frame <- data.frame(
      event = rep(1:length(events), each=length(seq(-pre,post,bin))),
      time = seq(-pre,post,bin),
      count = 0)
  } else { # With groups
    histogram_frame <- data.frame(
      event = rep(1:length(events), each=length(seq(-pre,post,bin))),
      group = rep(group, each=length(seq(-pre,post,bin))),
      time = seq(-pre,post,bin),
      count = 0)
  }
  
  # Populate the histogram data frame
  for (i in 1:length(events)) { # For each event..
    
    # Find this trial section (i.e., window surrounding event based on pre and post)
    start = events[i]-pre # Time point for start of pre-window
    end = events[i]+post+bin # Time point for end of post-window (make sure to include last bin)
    this_trial = spikes[spikes >= start & spikes <= end] # All spikes within trial i's window surrounding the event
    
    # Fill in the corresponding time bins
    for (j in 1:length(seq(-pre,post,bin))) { # For each bin..
      
      # Count the spikes for each time unit within this trial
      step_start = start+bin*(j-1)
      step_end = start+(bin*j)
      this_bin = length(this_trial[this_trial >= step_start & this_trial <= step_end]) # Number of spikes falling in bin j
      
      # Add spike count from trial i's bin j to data frame
      if (!this_bin == 0) {
        histogram_frame$count[histogram_frame$event == i][j] = this_bin
      }
    }
    
    # Remove binned spikes from list of all spikes, to prevent double-counting
    if (!allow_dupe) { spikes = setdiff(spikes,this_trial) }
    
  }

  # Adjust count data into spike rate
  if (is.null(group)) { # No groups
    n_spikes = sum(histogram_frame$count) # Total number of spikes
    histogram_frame$Hz = histogram_frame$count / bin # Spike rate (Hz)
    
  } else { # With groups
    group_labels = unique(group) # Names of the unique groups

    for (i in 1:length(group_labels)) { # Adjust data by subgroup
      
      # Add exception for group == NA
      if (is.na(group_labels[i])) {
        n_spikes = sum(histogram_frame$count[is.na(histogram_frame$group)]) # Total number of spikes within the i-th group
        histogram_frame$count_percentage[is.na(histogram_frame$group)] = histogram_frame$count[is.na(histogram_frame$group)] / n_spikes * 100 # Percentage of spikes falling in a given bin in i-th group
        histogram_frame$Hz[is.na(histogram_frame$group)] = histogram_frame$count[is.na(histogram_frame$group)] / bin # Spike rate (Hz) in i-th group
        
      } else {
        n_spikes = sum(histogram_frame$count[!is.na(histogram_frame$group) & histogram_frame$group == group_labels[i]]) # Total number of spikes within the i-th group
        histogram_frame$count_percentage[!is.na(histogram_frame$group) & histogram_frame$group == group_labels[i]] = histogram_frame$count[!is.na(histogram_frame$group) & histogram_frame$group == group_labels[i]] / n_spikes * 100 # Percentage of spikes falling in a given bin in i-th group
        histogram_frame$Hz[!is.na(histogram_frame$group) & histogram_frame$group == group_labels[i]] = histogram_frame$count[!is.na(histogram_frame$group) & histogram_frame$group == group_labels[i]] / bin # Spike rate (Hz) in i-th group
      }
    }
  }
  
  # Done!
  return(histogram_frame)
}