# Source
message("h.graph.perievent :: v0.6: 2023 Jan 31")

# Function
h.graph.perievent <- function(events, spikes, group = NULL,
                              pre = 5, post = 5, bin = .1,
                              plot = FALSE, plot_y = "count",
                              allow_dupe = TRUE, theme = FALSE) {
  
  # v0.6: 2023 Jan 31
  
  # Make a peri-event histogram around events (events, in seconds into global time),
  # with a pre-event window (pre, in seconds, default = 5 s),
  # and post-event window (post, in seconds, default = 5 s),
  # with a given bin size (bin, in seconds, default = 0.1 s),
  # for time points where spikes occurred (spikes, in seconds into global time).
  # If there are trial conditions, provide them as an ordered list (group),
  # where index matches the index for the ordered events (i.e. group[i] is for events[i])
  # Make a plot? plot=TRUE
  # Divide each bin count by the total number of counts to get percentages? proportional = TRUE
  
  # Check args
  if (!allow_dupe) { message("Are you sure you want to remove counted spikes (thus, no duplicates)? That is often not recommended.") }
  
  # Initialize histogram data frame
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
  if (theme == TRUE) {
    theme_set(theme(panel.background = element_rect(fill = "transparent", colour = NA),
                    plot.background = element_rect(fill = "transparent", colour = NA),
                    text = element_text(size=text_size)))
  }
  
  # Make plot?
  if (plot == TRUE) {
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
    return(histogram_frame)
  }
}