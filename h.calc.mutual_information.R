# Source
message("h.calc.mutual_information :: v0.1: 2026 May 31")

h.calc.mutual_information <- function(spiketrain, events, max_count = NULL, correct_bias = TRUE,
                                      shuffle = FALSE, n_shuffles = 10000, seed = 100,
                                      event_window = c(0,2), base_window = c(-3,-1)) {
  
  "
  Compute mutual information for a single neuron and assess significance via a shuffle test.
  
    Args:
      spiketrain (vector)     : Timestamps of spikes.
      events (vector)         : Timestamps of events of interest (like stimulus onset).
      max_count (int)         : The maximum cap on spike counts within event or baseline windows.
      shuffle (bool)          : Do a (label) shuffle test?
      correct_bias (bool)     : Perform a Panzeri-Treves bias correction? (Panzeri & Treves, 1996)
      n_shuffles (int)        : If shuffle = TRUE, the number of shuffle iterations.
      seed (int)              : Seed for shuffle test.
      event_window (vector)   : Start and end of the window within which we consider the stimulus' impact
                                (in seconds; e.g., event_window = c(0,2) looks at first 2 seconds post stimulus).
      base_window (vector)    : Start and end of the baseline window (in seconds; e.g., base_window = c(-3,-1)).
      
    Return:
      List of metrics         : mutual information, bias, event_spikecounts, baseline_spikecounts,
                                p-value, z-score, null distribution

  "

  #==========================================
 
  # Initialize
  obs_mi                 <- 0
  bias                   <- NULL
  n_events               <- length(events)
  spike_counts           <- array(0, dim=c(n_events,3))
  colnames(spike_counts) <- c('Trial','Events','Baseline')
  spike_counts[,'Trial'] <- 1:n_events
  
  # Count the number of spikes in each event window and baseline window
  for (i in 1:n_events) {
    
    # Trial events
    start <- events[i]+event_window[1]   # Time point for start of pre-window
    end   <- events[i]+event_window[2]   # Time point for end of post-window
    spike_counts[i,'Events']   <- length(spiketrain[spiketrain >= start & spiketrain <= end]) # All spikes within event i's window surrounding the event
    
    # Baseline events
    start <- events[i]+base_window[1]
    end   <- events[i]+base_window[2]
    spike_counts[i,'Baseline'] <- length(spiketrain[spiketrain >= start & spiketrain <= end])
    
  }
  
  # Cap the counts
  if (is.null(max_count)) {
    max_count <- max(c(spike_counts[,'Events'],spike_counts[,'Baseline']))
  } else { 
    spike_counts[,'Events']   <- pmin(spike_counts[,'Events'], max_count)
    spike_counts[,'Baseline'] <- pmin(spike_counts[,'Baseline'], max_count)
  }
  
  # Joint probability table of the spike counts
  joint <- matrix(0, nrow = 2, ncol = max_count + 1) # Row 1 = event, Row 2 = baseline
  ev_tab <- table(factor(spike_counts[,'Events'], levels = 0:max_count))
  bl_tab <- table(factor(spike_counts[,'Baseline'], levels = 0:max_count))
  joint[1, ] <- as.numeric(ev_tab)
  joint[2, ] <- as.numeric(bl_tab)
  
  # Normalize to joint probability
  p_joint <- joint / sum(joint)
  p_s     <- rowSums(p_joint)
  p_r     <- colSums(p_joint)
  
  # Mutual information (observed in the real data)
  for (i in 1:2) {
    for (j in seq_len(max_count + 1)) {
      if (p_joint[i, j] > 0 && p_s[i] > 0 && p_r[j] > 0) {
        obs_mi <- obs_mi + p_joint[i, j] * log2(p_joint[i, j] / (p_s[i] * p_r[j]))
      }
    }
  }
  
  # Bias correction
  'Panzeri, S and Treves, A (1996) “Analytical estimates of limited sampling biases in different infromation measures”'
  'S Panzeri, R Senatore, MA Montemurro and RS Petersen (2007) “Correcting for the sampling bias problem in spike train information measures”'
  if (correct_bias) {
    
    # Initialize
    bias        <- 0
    
    # Occupied bins in marginal response
    all_counts  <- c(spike_counts[,'Events'], spike_counts[,'Baseline'])
    n_counts    <- length(all_counts)
    r_tab       <- table(factor(all_counts, levels = 0:max_count))
    occupied_r  <- sum(r_tab > 0)
    
    # Occupied bins per state
    ev_tab      <- table(factor(spike_counts[,'Events'], levels = 0:max_count))
    bl_tab      <- table(factor(spike_counts[,'Baseline'], levels = 0:max_count))
    occupied_rs <- sum(ev_tab > 0) + sum(bl_tab > 0)
    
    # Compute bias
    bias <- (occupied_rs - occupied_r) / (2 * n_counts * log(2))
    bias
    
    # Adjust observed mutual information
    obs_mi <- max(obs_mi - bias, 0)
  }
  
  # Shuffle test
  if (shuffle) {
    
    # fx
    mutual_information <- function(event_counts, base_counts, max_count = NULL) {
      # event_counts: spike counts per trial in event window
      # base_counts:  spike counts per trial in baseline window
      # max_count:    cap on spike counts (NULL = observed max)
      #
      # Treats this as a two-state problem:
      #   S = {event, baseline}, R = spike count
      # The joint table has 2 rows (states) and max_count+1 columns.
      
      all_counts <- c(event_counts, base_counts)
      if (is.null(max_count)) {
        max_count <- max(all_counts)
      }
      
      ev_capped <- pmin(event_counts, max_count)
      bl_capped <- pmin(base_counts, max_count)
      
      # Build joint count table
      # Row 1 = event, Row 2 = baseline
      joint <- matrix(0, nrow = 2, ncol = max_count + 1)
      ev_tab <- table(factor(ev_capped, levels = 0:max_count))
      bl_tab <- table(factor(bl_capped, levels = 0:max_count))
      joint[1, ] <- as.numeric(ev_tab)
      joint[2, ] <- as.numeric(bl_tab)
      
      # Normalise to joint probability
      p_joint <- joint / sum(joint)
      p_s     <- rowSums(p_joint)
      p_r     <- colSums(p_joint)
      
      mi <- 0.0
      for (i in 1:2) {
        for (j in seq_len(max_count + 1)) {
          if (p_joint[i, j] > 0 && p_s[i] > 0 && p_r[j] > 0) {
            mi <- mi + p_joint[i, j] * log2(p_joint[i, j] / (p_s[i] * p_r[j]))
          }
        }
      }
      
      mi
    }
    panzeri_treves_correction <- function(event_counts, base_counts, max_count = NULL) {
      
      all_counts <- c(event_counts, base_counts)
      if (is.null(max_count)) max_count <- max(all_counts)
      
      ev_capped <- pmin(event_counts, max_count)
      bl_capped <- pmin(base_counts, max_count)
      n_total   <- length(all_counts)
      
      # Occupied bins in marginal response
      all_capped <- c(ev_capped, bl_capped)
      r_tab      <- table(factor(all_capped, levels = 0:max_count))
      occupied_r <- sum(r_tab > 0)
      
      # Occupied bins per state
      ev_tab      <- table(factor(ev_capped, levels = 0:max_count))
      bl_tab      <- table(factor(bl_capped, levels = 0:max_count))
      occupied_rs <- sum(ev_tab > 0) + sum(bl_tab > 0)
      
      bias <- (occupied_rs - occupied_r) / (2 * n_total * log(2))
      bias
    }
    
    # Seed
    set.seed(seed)
    
    # Null distribution: for each shuffle, independently flip each trial's event/baseline labels with probability 0.5
    null_mi <- numeric(n_shuffles)
    
    for (i in seq_len(n_shuffles)) {
      flips      <- sample(c(TRUE, FALSE), n_events, replace = TRUE)
      shuf_event <- ifelse(flips, spike_counts[,'Baseline'], spike_counts[,'Events'])
      shuf_base  <- ifelse(flips, spike_counts[,'Events'], spike_counts[,'Baseline'])
      
      raw <- mutual_information(shuf_event, shuf_base, max_count)
      if (correct_bias) {
        b   <- panzeri_treves_correction(shuf_event, shuf_base, max_count)
        raw <- max(raw - b, 0)
      }
      null_mi[i] <- raw
    }
    
    # Statistics
    p_value <- mean(null_mi >= obs_mi)
    z_score <- if (sd(null_mi) > 0) {
      (obs_mi - mean(null_mi)) / sd(null_mi)
    } else {
      0
    }
    
  }
  
  # Finished!
  if (shuffle) { 
    return(list(mutual_info          = obs_mi,
                bias                 = bias,
                event_spikecounts    = spike_counts[,'Events'],
                baseline_spikecounts = spike_counts[,'Baseline'],
                p_value              = p_value,
                z_score              = z_score,
                null_distr           = null_mi))
  } else { 
    return(list(mutual_info          = obs_mi,
                bias                 = bias,
                event_spikecounts    = spike_counts[,'Events'],
                baseline_spikecounts = spike_counts[,'Baseline']))
  }
  
  
}