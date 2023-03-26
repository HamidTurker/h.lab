# Source
message("h.calc.rugosity :: v0.2: 2023 Feb 21")

# Function
h.calc.rugosity <- function(events=NULL, smthd_curve=NULL, curve_events=NULL, total_dur, bw_method, bw_adj,
                            kernel = "gaussian", density = 1, min_density = TRUE) {
  
  # v0.2: 2023 Feb 21
  
  
  # Are the data smoothed already?
  if (!is.null(smthd_curve)) { # Already smoothed..
    
    # Actual density
    actual.dens = smthd_curve
    
    # Minimal density
    if (min_density) {
      if (exists('curve_events')) {
        message("Computing minimal rugosity using provided events underpinning smoothed curve.")
        
        n_events = length(curve_events)
        uniform_space = seq(from = 0, to = total_dur, length.out = n_events)
        minimal.dens = density(uniform_space, bw = bw, adjust = bw_adj, kernel = kernel, n = total_dur*density, from = 1/density, to = total_dur)$y
        
      } else {
        message("Computing minimal rugosity by assuming 3 events/second. Provide your own list of events (used to make the smoothed curve), if possible.")
        
        n_events = ceiling(total_dur/3)
        uniform_space = seq(from = 0, to = total_dur, length.out = n_events)
        minimal.dens = density(uniform_space, bw = bw, adjust = bw_adj, kernel = kernel, n = total_dur*density, from = 1/density, to = total_dur)$y
        
      }
    }
    
    
    
  } else { # Not smoothed yet, so will need to do that too..
    
    # Total number of events
    n_events = length(events)
    
    # Evenly spaced events
    uniform_space = seq(from = 0, to = total_dur, length.out = n_events)
    
    # Actual and minimal density
    temp = density(events, bw = bw, adjust = bw_adj, kernel = kernel, n = total_dur*density, from = 1/density, to = total_dur)
    
    pad.ts <- ceiling(temp$bw * 2)
    actual.dens = density(events, bw = bw, adjust = bw_adj, kernel = kernel, n = ceiling((total_dur+2*pad.ts)*density), from = 1/density - pad.ts, to = total_dur+pad.ts)$y
    minimal.dens = density(uniform_space, bw = bw, adjust = bw_adj, kernel = kernel, n = ceiling((total_dur+2*pad.ts)*density), from = 1/density - pad.ts, to = total_dur+pad.ts)$y
  
  } # Smoothed end
  
  
  
  # Rugosity of the actual events
  n_dens <- length(actual.dens)
  y_dens <- numeric(n_dens - 1)
  for (i in 1:(n_dens - 1)) {
    y_dens[i] <- (actual.dens[i + 1] - actual.dens[i])^2
  }
  actual_rugosity <- sqrt(mean(y_dens))
  
  # Rugosity of the minimal density
  if (min_density) {
    n_dens <- length(minimal.dens)
    y_dens <- numeric(n_dens - 1)
    for (i in 1:(n_dens - 1)) {
      y_dens[i] <- (minimal.dens[i + 1] - minimal.dens[i])^2
    }
    minimal_rugosity <- sqrt(mean(y_dens))
    
    # Peakiness
    peakiness = actual_rugosity / minimal_rugosity
    
    # Finished!
    return(c(actual_rugosity, minimal_rugosity, peakiness))
  } else {
    return(actual_rugosity)
  }
}


