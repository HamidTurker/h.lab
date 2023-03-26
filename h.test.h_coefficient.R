h.test.h_coefficient <- function(perievents, rawdata, mean_FR = 3, n_events = 60,
                                 resp_window = c(.2,1), fast = TRUE, kern_conf = .95) {
  
  "
  Compute h-coefficient based on peri-event histograms.
  
  Based on Michael Hill's expsim in the h-coefficient toolbox for Matlab.
  If you use this function, you must cite https://journals.physiology.org/doi/full/10.1152/jn.00595.2014
  "
  
  # Initialize
  resp_dur = resp_window[2]-resp_window[1]
  
  
  
  
}


randshuff <- function(n_events, resp_dur, rawdata, mean_FR, fast = TRUE, samplerate = 1e3, n_shuff = 1000) {
  
  "
  Take all the spikes from an entire session (rawdata) and create n_shuff random shuffles of responses,
  irrespective of whether there was a real neural response or not. To each of these shuffled responses,
  this function then applies adaptive kernel smoothing (ssvkernel) and calculates 'master response' M
  across all shuffles. This master response is then used to calculate the h-coefficient.
  
  To execute ssvkernel 1000 times takes long (default n_shuff = 1000). So, randshuff also offers a 'fast'
  method (fast=TRUE), which cuts out 100 segments that are 10 times as long. The smoothing may not be as
  good as when taking 1000 segments, but should be approximately similar as long as your interstimulus
  intervals are at least 10 times longer than your baseline periods.
  
    Args:
      n_events (int)      : The number of events/stimuli/trials in the experiment.
      resp_dur (scalars)  : The difference between the beginning and end of the response window in the post-event period.
                            For instance, resp_dur = c(.2,1). If using simulated data, ensure this matches the simulation.
      rawdata (vector)    : Vector containing timestamps for each spike in the session.
      mean_FR (scalar)    : Mean firing rate for this neuron across the whole session.
      fast (bool)         : Run the simplified-but-faster (fast = TRUE) or more-precise-but-slower (fast = FALSE) shuffle method.
      samplerate (scalar) : Sample rate in your data, in Hz. If using simulated data, ensure this matches the simulation.
      n_shuff (int)       : Explicitly set the number of shuffles, if desired. This gets divided by 10 if fast=TRUE.
                            Thus, if fast=TRUE, n_shuff must be a multiple of 10.
      kern_conf (scalar)  : Desired confidence interval for the ssv_kernel (e.g., kern_conf = .95)
  "
  
  # Check args
  if (fast & !n_shuff%%10 == 0) { stop("You requested the fast method, but n_shuff is not a multiple of 10.") }
  if (!kern_conf < 1 | !kern_conf > 0) { stop("Requested kern_conf must be between 0 and 1, exclusive (e.g., kern_conf = .95).") }
  
  # Set seed
  set.seed(Sys.time())
  
  # Initialize
  #resp_dur = resp_dur*samplerate
  padding = 2*samplerate            # Padding against edge artifacts produced by kernel smoothing, which can be deleted afterwards
  kernshuff = sample(n_shuff)
  shuffsy = array(NaN, c(n_events,1))
  n_segs = floor((floor(rawdata[length(rawdata)])-(2*samplerate) - ceiling(rawdata[1])) / (resp_dur*10))

  # Shuffle
  if (fast) {
    message("Shuffling & smoothing with the fast (but less accurate) method.")
    
    for (i in 1:(n_shuff/10)) { # For each shuffle..
      message(paste0("Working on ",(((i-1)*10)+1)," to ",(i*10)," out of ",n_shuff," shuffles..." ))
      perm_idx1 = sample(n_segs*10)
      perm_idx2 = sample(n_segs)
      
      for (j in 1:n_events) { # For each event..
        shuffsy[j] = length(rawdata[rawdata > ((perm_idx1[j]-1)*resp_dur) & rawdata <= (((perm_idx1[j]-1)*resp_dur)+resp_dur)])

        # Prep adaptive kernel analysis
        hold = rawdata[rawdata > ((perm_idx2[j]-1)*(resp_dur*10)) & rawdata <= ((perm_idx2[j]-1)*(resp_dur*10))+(resp_dur*10)+padding] - ((perm_idx2[j]-1)*(resp_dur*10))
        if (j == 1) {
          forkern = hold
        } else {
          forkern = c(forkern, hold)
        }
      }
      
      # Adaptive kernel analysis
      prekern <- h.calc.ssv_kernel(rawdata = (forkern*(10^-(log(samplerate, base=10)))), tin = seq(0.01,(((resp_dur*10)+padding)/samplerate),by=.01), W = NULL, samplerate = samplerate, confidence = kern_conf)
      
    }
    
    
    
    
  } else {
    message("Shuffling & smoothing with the slow (but more accurate) method.")
    
  }
  
}