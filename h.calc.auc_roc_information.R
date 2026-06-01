# Source
message("h.calc.auc_roc_information :: v0.1: 2026 June 1")

h.calc.auc_roc_information <- function(spiketrain, events, shuffle = FALSE, n_shuffles = 10000, 
                                       seed = 100, event_window = c(0,2), base_window = c(-3,-1), alternative = "two.sided") {
  
  "
  Compute AUC / ROC information for a single neuron's stimulus-evoked response, and assess significance via a shuffle test (within-trial label flip).
  
  For each trial, we have a spike count from an event window and a spike count from a baseline window. The ROC curve asks: if
  given a single spike count, how well could an ideal observer tell whether it came from the event or baseline state?

  AUC = 0.5 means chance (identical distributions)
  AUC = 1.0 means perfect separation (stimulus event always higher than baseline)
  AUC = 0.0 means perfect separation (stimulus event always lower than baseline)
 
  AUC is equivalent to the probability that a randomly drawn event count exceeds a randomly drawn baseline count (the common-
  language effect size, or Mann-Whitney U statistic normalised by n_events * n_baselines).
 

    Args:
      spiketrain (vector)     : Timestamps of spikes.
      events (vector)         : Timestamps of events of interest (like stimulus onset).
      shuffle (bool)          : Do a (label) shuffle test?
      n_shuffles (int)        : If shuffle = TRUE, the number of shuffle iterations.
      seed (int)              : Seed for shuffle test.
      event_window (vector)   : Start and end of the window within which we consider the stimulus' impact
                                (in seconds; e.g., event_window = c(0,2) looks at first 2 seconds post stimulus).
      base_window (vector)    : Start and end of the baseline window (in seconds; e.g., base_window = c(-3,-1)).
      alternative (char)      : Alternative hypothesis, for shuffle testing ('two.sided', 'greater', or 'less').
      
    Return:
      List of metrics         : AUC/ROC info, event_spikecounts, baseline_spikecounts,
                                p-value, z-score, null distribution, H_alternative

  "

  
  # Initialize
  obs_auc                <- 0
  obs_roc                <- 0
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
  
  # Thresholds: every unique count value, plust one above the max
  all_counts <- sort(unique(c(spike_counts[,'Events'], spike_counts[,'Baseline'])))
  thresholds <- c(max(all_counts) + 1, rev(all_counts))
  
  tpr <- numeric(length(thresholds))
  fpr <- numeric(length(thresholds))
  
  for (i in seq_along(thresholds)) {
    thr    <- thresholds[i]
    tpr[i] <- sum(spike_counts[,'Events'] >= thr) / n_events
    fpr[i] <- sum(spike_counts[,'Baseline'] >= thr) / n_events
  }
  
  # AUC via trapezoidal rule
  for (i in 2:length(fpr)) {
    obs_auc <- obs_auc + (fpr[i] - fpr[i - 1]) * (tpr[i] + tpr[i - 1]) / 2
  }


  
  # Shuffle test
  if (shuffle) {
    
    # fx
    compute_roc_auc <- function(event_counts, base_counts) {
      # Treats event as "positive" class and baseline as "negative".
      # At each threshold, TPR = fraction of event counts >= threshold,
      # FPR = fraction of baseline counts >= threshold.
      #
      # Returns:
      #   auc         - area under the ROC curve
      #   thresholds  - threshold values used
      #   tpr         - true positive rate at each threshold
      #   fpr         - false positive rate at each threshold
      #   n_event     - number of event observations
      #   n_base      - number of baseline observations
      
      n_event <- length(event_counts)
      n_base  <- length(base_counts)
      
      # Thresholds: every unique count value, plus one above the max
      all_counts  <- sort(unique(c(event_counts, base_counts)))
      thresholds  <- c(max(all_counts) + 1, rev(all_counts))
      
      tpr <- numeric(length(thresholds))
      fpr <- numeric(length(thresholds))
      
      for (i in seq_along(thresholds)) {
        thr    <- thresholds[i]
        tpr[i] <- sum(event_counts >= thr) / n_event
        fpr[i] <- sum(base_counts >= thr) / n_base
      }
      
      # AUC via trapezoidal rule
      auc <- 0
      for (i in 2:length(fpr)) {
        auc <- auc + (fpr[i] - fpr[i - 1]) * (tpr[i] + tpr[i - 1]) / 2
      }
      
      list(
        auc        = auc,
        thresholds = thresholds,
        tpr        = tpr,
        fpr        = fpr,
        n_event    = n_event,
        n_base     = n_base
      )
    }
    
    # Seed
    set.seed(seed)
    
    # Null distribution: for each shuffle, independently flip each trial's event/baseline labels with probability 0.5
    null_auc <- numeric(n_shuffles)
    
    for (i in seq_len(n_shuffles)) {
      flips    <- sample(c(TRUE, FALSE), n_events, replace = TRUE)
      shuf_ev  <- ifelse(flips, spike_counts[,'Baseline'], spike_counts[,'Events'])
      shuf_bl  <- ifelse(flips, spike_counts[,'Events'], spike_counts[,'Baseline'])
      null_auc[i] <- compute_roc_auc(shuf_ev, shuf_bl)$auc
    }
    
    # Statistics
    p_value <- switch(alternative,
                      "two.sided" = mean(abs(null_auc - 0.5) >= abs(obs_auc - 0.5)),
                      "greater"   = mean(null_auc >= obs_auc),
                      "less"      = mean(null_auc <= obs_auc),
                      stop("alternative must be 'two.sided', 'greater', or 'less'")
    )
    z_score <- if (sd(null_auc) > 0) {
      (obs_auc - mean(null_auc)) / sd(null_auc)
    } else {
      0
    }
    
    
  }
  
  # Finished!
  if (shuffle) { 
    return(list(auc_info             = obs_auc,
                event_spikecounts    = spike_counts[,'Events'],
                baseline_spikecounts = spike_counts[,'Baseline'],
                p_value              = p_value,
                z_score              = z_score,
                null_distr           = null_auc,
                H_alternative        = alternative))
  } else { 
    return(list(auc_info             = obs_auc,
                event_spikecounts    = spike_counts[,'Events'],
                baseline_spikecounts = spike_counts[,'Baseline']))
  }
  
  
}