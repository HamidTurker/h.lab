# Source
message("h.calc.skaggs_information :: v0.1: 2026 May 1")

h.calc.skaggs_information <- function(type = 'stimulus', spiketrain, events, samplerate = NULL,
                                      shuffle = FALSE, n_shuffles = 10000, seed = 100,
                                      event_window = c(0,2), base_window = c(-3,-1), global_base = FALSE) {
  
  "
  Compute Skaggs information (1993) for a single neuron and assess significance via a shuffle test.
  
    Args:
      type (char)             : Can be 'stimulus', 'states', or 'descriptives'
      
      [if type = 'stimulus']
      spiketrain (vector)     : Timestamps of spikes.
      events (vector)         : Timestamps of events of interest (like stimulus onset).
      shuffle (bool)          : Do a (circular-shift) shuffle test?
      n_shuffles (int)        : If shuffle = TRUE, the number of shuffle iterations.
      seed (int)              : Seed for shuffle test.
      event_window (vector)   : Start and end of the window within which we consider the stimulus' impact
                                (in seconds; e.g., event_window = c(0,2) looks at first 2 seconds post stimulus).
      base_window (vector)    : Start and end of the baseline window (in seconds; e.g., base_window = c(-3,-1)).
                                If base_window = NULL, all time points outside of event_window will be used (which
                                means global_base = TRUE).    
      global_base (bool)      : Compute the baseline firing rate using all available data (=TRUE) outside of the event_windows,
                                or using a predefined base_window (=FALSE). A global baseline is perhaps better if there are no
                                other stimulus or behavioral events throughout the recording. A specific baseline window is perhaps
                                better if other stimulus or behavioral events were also taking place during the recording, and you don't
                                want the spike data at those moments in time to impact this measure.

      
      [if type = 'states']
      spiketrain (vector)     : Numeric vector, number of spikes at each corresponding time bin.
      events (vector)         : Numeric vector, states at each given time bin.
      samplerate (int)        : The sample rate of the data (in seconds).
      shuffle (bool)          : Do a (circular-shift) shuffle test?
      n_shuffles (int)        : If shuffle = TRUE, the number of shuffle iterations.
      seed (int)              : Seed for shuffle test.
                                
      # For example, an animal spends 500 ms in state 1 and there is 1 spike, followed by another 500 ms in state 1 with 0 spikes,
      # then 500 ms in state 2 with 45 spikes, then state 6 with 3 spikes:
      # spiketrain = c(1,0,45,3,...)
      # events = c(1,1,2,6,...)
      # samplerate = .5
      
                                
      [if type = 'descriptives']
      events (vector)         :  numeric vector, proportion of overall time spent in given state (does not need to sum to 1)
      spiketrain (vector)     :  numeric vector, mean firing rate (Hz) in the corresponding state
      
      # For example, an animal spent 43% of the session in state 1, 10% in state 2, 27% in state 3, 20% in state 4. We observed
      # an average of 10.6 Hz, 3 Hz, 8 Hz, and 7.3 Hz firing per corresponding state.
      # events = c(.43,.1,.27,.2)
      # spiketrain = c(10.6,3,8,7.3)
      
    Return:
      List of metrics         : bits per spike, bits per second, p-value, z-score

  "
  
  # Stimulus-based setup
  if (type == 'stimulus') {
    
    # fx
    decimal_places <- function(x) {
      # Validate input
      if (!is.numeric(x)) {
        stop("Input must be numeric.")
      }
      
      sapply(x, function(num) {
        # Handle NA values
        if (is.na(num)) return(NA_integer_)
        
        # Convert to character without scientific notation
        num_str <- format(num, scientific = FALSE, trim = TRUE)
        
        # Split at decimal point
        parts <- strsplit(num_str, "\\.")[[1]]
        
        # If no decimal part, return 0
        if (length(parts) == 1) return(0L)
        
        # Count characters after decimal
        nchar(parts[2])
      })
    }
    
    # Compute delta time decimal precision required
    if (!is.null(base_window)) { dec_prec <- c(event_window,base_window) } else { dec_prec <- event_Window }
    max_dec_prec <- max(decimal_places(dec_prec))
    if (max_dec_prec == 0)  { dt <- 1 } else { dt <- 1 / (10*max_dec_prec) }
    
    # Setup
    buffer        <- 10 # zero-padding after the last recorded spike time stamp (in seconds)
    n_spikes      <- length(spiketrain)
    n_events      <- length(events)
    last_spike    <- max(spiketrain) # last recorded spike
    n_bins        <- 1 + ceiling(last_spike + buffer) / dt
    event_bins    <- seq(event_window[1], event_window[2], by=dt)
    base_bins     <- seq(base_window[1], base_window[2], by=dt)
    
    # Initialize
    binned_data     <- array(0, dim=c(n_bins,3))
    binned_data[,1] <- seq(0, n_bins-1, by=dt) # Time points
    
    # Mark the bins
    for (i in 1:(n_bins-1)) {
      binned_data[i,2] <- length(spiketrain[spiketrain >= binned_data[i,1] & spiketrain <= binned_data[i+1,1]])
    }
    binned_data[,2] <- binned_data[,2] / dt # Convert to Hz
    
    # Mark the event and baseline windows
    if (global_base) { binned_data[,3] <- -1 }
    for (i in 1:n_events) {
      # Events
      this_window  <- events[i] + event_bins
      first_winbin <- max(which(binned_data[,1] < min(this_window)))
      last_winbin  <- max(which(binned_data[,1] < max(this_window)))
      binned_data[first_winbin:last_winbin,3] <- 1
      
      # Baselines
      if (!global_base) {
        this_base  <- events[i] + base_bins
        first_basebin <- max(which(binned_data[,1] < min(this_base)))
        last_basebin  <- max(which(binned_data[,1] < max(this_base)))
        binned_data[first_basebin:last_basebin,3] <- -1
      }
    }
    colnames(binned_data) <- c('Time','Hz','State')
    
    # Sub-set the data if we don't have a global_base
    binned_data <- binned_data[!binned_data[,3] == 0,] 
    n_bins      <- length(binned_data[,3])

    # Compute observed state occupancy & corresponding firing rates
    obs_nstates   <- dim(table(binned_data[,3]))
    obs_occupancy <- c(as.numeric(table(binned_data[,3])[2]), as.numeric(table(binned_data[,3])[1])) / obs_nstates
    obs_rates     <- c(mean(binned_data[binned_data[,3] == 1,2]), mean(binned_data[binned_data[,3] == -1,2]))
    
    # Additional information to keep, for the shuffle test
    vec_spikes  <- binned_data[,2]
    vec_states  <- binned_data[,3]
  }
  
  # State-based setup
  if (type == 'states') { 
    # If we have data in the occupancy+rate vector format, we can bypass the type=='stimulus' section above
    message("The shuffle test runs a circular shift, so spike and event vectors MUST be in chronological order!")
    if (length(spiketrain) == length(events)) { stop("The spiketrain and event vectors are not of the same length.")}
    if (is.null(samplerate)) { stop("You must provide a sample rate value: e.g., 'each bin equals 1 sec -> samplerate = 1'.")}
    
    # Setup
    n_bins          <- length(events)
    
    # Initialize
    binned_data     <- array(0, dim=c(n_bins,3)); colnames(binned_data) <- c('Time','Hz','State')
    binned_data[,1] <- seq(0, n_bins-1, by=samplerate)
    binned_data[,2] <- spiketrain
    binned_data[,3] <- events
    
    # Compute observed state occupancy & corresponding firing rates
    obs_nstates   <- dim(table(binned_data[,3]))
    obs_occupancy <- c(as.numeric(table(binned_data[,3])[2]), as.numeric(table(binned_data[,3])[1])) / obs_nstates
    obs_rates     <- c(mean(binned_data[binned_data[,3] == 1,2]), mean(binned_data[binned_data[,3] == -1,2]))
    
    # Additional information to keep, for the shuffle test
    vec_spikes  <- binned_data[,2]
    vec_states  <- binned_data[,3]
  }
  
  # Descriptives setup
  if (type == 'descriptives') { 
    # If we have data in the summarized format, a shuffle test is not recommended, because we've lost chronological (spike autocorrelation) info.
    if (!length(spiketrain) == length(events)) { stop("The rate and proportional occupancy vectors are not of the same length.")}
    shuffles      <- FALSE
    obs_nstates   <- length(events)
    p             <- events
    obs_rates     <- spiketrain
  }
  
  ### Skaggs computation
  {
    if (!type == 'descriptives') {
      p        <- obs_occupancy / sum(obs_occupancy) # Proportion of time spent in given state
    }
    lambda_bar <- sum(p * obs_rates)                 # Average firing rate, weighted by time spent each state
    if (lambda_bar == 0) {                           # If there was no firing, there's no information
      bits_per_spike <- 0
      bits_per_sec   <- 0
    } else {                                         # ..else, compute the information content
      
      # Only bins with positive rate and occupancy contribute
      valid <- obs_rates > 0 & p > 0                 # Only select those observations, where we have spike data and occupancy
      ratio <- obs_rates[valid] / lambda_bar         # Ratio between observed spike rate and average spike rate
      
      # Finished!
      bits_per_sec   <- sum(p[valid] * obs_rates[valid] * log2(ratio))
      bits_per_spike <- bits_per_sec / lambda_bar
    }
  }
  
  # Shuffle test
  if (shuffle) {
    
    # To run the shuffle, we require n_bins, obs_nstates, obs_occupancy, & obs_rates
    
    # Seed
    set.seed(seed)
    
    # fx
    compute_rates <- function(stim, spikes) {
      states <- sort(unique(stim))
      rates  <- numeric(length(states))
      occ    <- numeric(length(states))
      for (k in states) {
        idx       <- stim == states[k] # All bins for the k-th state
        occ[k]    <- sum(idx) * dt     # Total occupancy in k-th state (in seconds)
        rates[k]  <- if (occ[k] > 0) sum(spikes[idx]) / occ[k] else 0
      }
      p <- occ / sum(occ)
      list(rates = rates, occupancy = occ, p = p)
    }
    
    # Null distribution: circular shift should break any stimulus/state-spike coupling,
    # while preserving spike-train temporal autocorrelation. It does not matter whether
    # we compute the metrics using bits_per_spike or bits_per_sec, it will be the same result.
    null_info <- numeric(n_shuffles);
    min_shift <- max(20, round(0.05 * n_bins))
    for (i in (1:n_shuffles)) {
      shift            <- sample(min_shift:(n_bins - min_shift), 1)
      shuffled_states  <- c(tail(vec_states, n_bins - shift),
                            head(vec_states, shift))
      res              <- compute_rates(shuffled_states, vec_spikes)
      null_out         <- h.calc.skaggs_information(spiketrain = res$rates,
                                  events = res$p,
                                  type = 'descriptives', shuffle = FALSE)
      null_info[i] <- null_out[[1]]
    }
    

    # Finished!
    p_value <- mean(null_info >= bits_per_spike)
    z_score <- (bits_per_spike - mean(null_info)) / sd(null_info)
    
  }
  
  # All finished!
  if (shuffle) { 
    return(list(bits_per_spike = bits_per_spike, 
                bits_per_sec   = bits_per_sec,
                p_value        = p_value,
                z_score        = z_score))
  } else { 
    return(list(bits_per_spike = bits_per_spike, 
                bits_per_sec   = bits_per_sec)) 
  }
  
  
}