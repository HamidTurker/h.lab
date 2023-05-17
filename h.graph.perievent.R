# Source
message("h.graph.perievent :: v0.7: 2023 Mar 27")

# Function
h.graph.perievent <- function(events, spikes, group = NULL,
                              pre = 5, post = 5, bin = .1,
                              allow_dupe = TRUE, other_events = NULL, other_events_cols = NULL,
                              plot = FALSE, plot_y = "count", theme = FALSE) {
  
  "
  Make a peri-event histogram around events (events, in seconds into global time),
  with a pre-event and post-event window, with a given bin size.
  
    Args:
      events (vector)         : Vector of event times (numeric) for events of interest.
      spikes (vector)         : Vector of time points where spikes occurred (numeric, in seconds).
      group (vector)          : Vector where each entry notes the group label of the corresponding entry in the event vector.
      pre (num)               : Pre-event window (in seconds).
      post (num)              : Post-event window (in seconds).
      allow_dupe (bool)       : Allow a spike to be counted multiple times if it falls within overlapping trial windows (allowing this is recommended).
      other_events (list)     : List of vectors of other event times to be marked (events which
                                are not the event being used as reference point at x=0). Each
                                vector in the list must have length equal to length(events) and ordered
                                to match the ordering of the events vector. If a given trial does not
                                have a certain other_event to be marked, leave that entry as NA.
      other_events_cols (char): Colors to mark the other events in (e.g., other_events_cols=c('red','green','blue'),
                                for 3 other events to be marked).
      plot (bool)             : Make a plot (TRUE) or just return the final data frame (FALSE)?
      plot_y (char)           : Can plot raw counts (plot_y = 'count') or percentage (plot_y = 'percentage').
      theme (bool)            : Set a transparent theme for the plot (theme = TRUE) or not (theme = FALSE).
      
      
    Return:
      A peri-event histrogram or data frame to create your own.
  "
  
  # Check args
  {
    if (!allow_dupe) { message("Are you sure you want to remove counted spikes (thus, no duplicates)? That is often not recommended.") }
    
    if (!is.null(other_events)) {
      if (length(other_events)==1) { other_events = list(other_events) }
      if (!class(other_events)=="list") { stop("Your other_events must be in a list of vectors.") }
      for (i in 1:length(other_events)) {
        if (!length(other_events[[i]])==length(events)) { 
          stop("Each provided other_event does not have a corresponding event. There has to be the same number of elements in each other_event vector as the main event vector.") }
      }
      if (!is.null(other_events_cols)) { 
        if (!length(other_events_cols)==length(other_events)) { stop("The number of other_events_cols doesn't match the number of other_event vectors.") }
        if (sortby_other_event > length(other_events)) { stop("The sortby_other_event index is larger than the number of other_event vectors.") }
      }
    }
    
    if (pre < 0) { pre = abs(pre) }
  }
    
  # Initialize histogram data frame
  n_events = length(events)
  if (is.null(group)) { # No groups
    histogram_frame <- data.frame(
      time = seq(-pre,post,bin),
      count = 0,
      count_percentage = 0,
      Hz = 0)
  } else { # With groups
    n_groups = length(unique(group))
    histogram_frame <- data.frame(
      group = rep(sort(unique(group)), each=length(seq(-pre,post,bin))),
      time = rep(seq(-pre,post,bin), n_groups),
      count = 0,
      count_percentage = 0,
      Hz = 0)
  }
  
  # Populate the histogram data frame
  if (is.null(group)) { # No groups
    for (i in 1:length(events)) { # For each event..
      
      # Find this trial section (i.e., window surrounding event based on pre and post)
      start = events[i]-pre # Time point for start of pre-window
      end = events[i]+post+bin # Time point for end of post-window (make sure to include last bin)
      this_trial = spikes[spikes >= start & spikes <= end] # All spikes within trial i's window surrounding the event
      
      # Fill in the corresponding time bins
      for (j in 1:length(histogram_frame$time)) { # For each bin..
        
        # Count the spikes for each time unit within this trial
        step_start = start+bin*(j-1)
        step_end = start+(bin*j)
        this_bin = length(this_trial[this_trial >= step_start & this_trial <= step_end]) # Number of spikes falling in bin j
        
        # Add spike count from trial i's bin j to data frame
        if (!this_bin == 0) {
          histogram_frame$count[j] = histogram_frame$count[j]+this_bin
        }
      }
      
      # Remove binned spikes from list of all spikes, to prevent double-counting
      if (!allow_dupe) { spikes = setdiff(spikes,this_trial) }
    }
    
  } else { # With groups
    for (i in 1:length(events)) { # For each event..
      
      # Which group does this event belong to?
      group_entry = group[i]
      
      # Find this trial section (i.e., window surrounding event based on pre and post)
      start = events[i]-pre # Time point for start of pre-window
      end = events[i]+post+bin # Time point for end of post-window (make sure to include last bin)
      this_trial = spikes[spikes >= start & spikes <= end] # All spikes within trial i's window surrounding the event
      
      # Fill in the corresponding time bins
      for (j in 1:length(unique(histogram_frame$time))) { # For each bin..

        # Which time bin?
        bin_entry = unique(histogram_frame$time)[j]
        
        # Count the spikes for each time unit within this trial
        step_start = start+bin*(j-1)
        step_end = start+(bin*j)
        this_bin = length(this_trial[this_trial >= step_start & this_trial <= step_end]) # Number of spikes falling in bin j
        
        # Add spike count from trial i's bin j to data frame
        if (!this_bin == 0) {
          
          # What's the current tally?
          current_count = histogram_frame$count[histogram_frame$group == group_entry &
                                                  histogram_frame$time == bin_entry]
          
          # Add new spikes to current spike count
          histogram_frame$count[histogram_frame$group == group_entry &
                                  histogram_frame$time == bin_entry] = current_count+this_bin
        }
      }
      
      # Remove binned spikes from list of all spikes, to prevent double-counting
      if (!allow_dupe) { spikes = setdiff(spikes,this_trial) }
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
  
  # Adjust count data into percentage and spike rate
  if (is.null(group)) { # No groups
    n_spikes = sum(histogram_frame$count) # Total number of spikes
    histogram_frame$count_percentage = histogram_frame$count / n_spikes * 100 # Percentage of spikes falling in a given bin
    histogram_frame$count_percentage[is.na(histogram_frame$count_percentage)] = 0 # Correct for any by-0 divisions
    histogram_frame$Hz = histogram_frame$count / (length(events)*bin) # Spike rate (Hz)
    
  } else { # With groups
    group_labels = unique(group) # Names of the unique groups
    
    for (i in 1:length(group_labels)) { # Adjust data by subgroup
      n_spikes = sum(histogram_frame$count[histogram_frame$group == group_labels[i]]) # Total number of spikes within the i-th group
      histogram_frame$count_percentage[histogram_frame$group == group_labels[i]] = histogram_frame$count[histogram_frame$group == group_labels[i]] / n_spikes * 100 # Percentage of spikes falling in a given bin in i-th group
      histogram_frame$count_percentage[is.na(histogram_frame$count_percentage)] = 0 # Correct for any by-0 divisions
      histogram_frame$Hz[histogram_frame$group == group_labels[i]] = histogram_frame$count[histogram_frame$group == group_labels[i]] / (sum(group==group_labels[i])*bin) # Spike rate (Hz) in i-th group
    }
  }
  
  # Set theme?
  if (theme) {
    theme_set(theme(panel.background = element_rect(fill = "transparent", colour = NA),
                    plot.background = element_rect(fill = "transparent", colour = NA),
                    text = element_text(size=text_size)))
  }
  
  # Make plot?
  if (plot) {
    if (plot_y == "count") { 
      return(ggplot(histogram_frame, aes(x=time, y=count)) + 
               geom_bar(stat='identity', colour="black") +
               theme(
                 panel.background = element_rect(fill = "transparent", colour = NA),
                 plot.background = element_rect(fill = "transparent", colour = NA)))
    } else if (plot_y == "percentage") {
      return(ggplot(histogram_frame, aes(x=time, y=count_percentage)) + 
               geom_bar(stat='identity', colour="black") +
               theme(
                 panel.background = element_rect(fill = "transparent", colour = NA),
                 plot.background = element_rect(fill = "transparent", colour = NA)))
    }
  } else {
    if (is.null(other_events)) { return(histogram_frame) } else { return(list(histogram_frame,other_event_times))}
  }
}