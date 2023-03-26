# Source
message("h.simulate.neuron_spiketrain :: v0.1: 2022 Dec 1")

# Function
h.simulate.neuron_spiketrain <- function(duration = 60, n_events = 60, event_window = c(-5,5,.001),
                                         base_window = c(-5,-.2), resp_window = c(.2,1), stim_time = 0,
                                         mean_FR = 3, amplitude = 2, resp_peak = .5, resp_perc = .6,
                                         sigma = .3, noise_freq = c(0,.1), noise_amp = c(.5,1),
                                         refract = 3, modulator = "window", verbose=TRUE) {
  
  "
  Simulate artificial spiking data for a single neuron's session, including 
  artificial trials/events to create raster plots and perievent histograms.
  
  Based on Michael Hill's expsim in the h-coefficient toolbox for Matlab.
  If you use this function, you must cite https://journals.physiology.org/doi/full/10.1152/jn.00595.2014
  
  
    Args:
      duration (scalar)       : Total duration of your simulated session (in minutes).
                                Ensure that this is long enough to create your desired number
                                of events & rasters / perievent plots without overlapping event_windows.
      n_events (int)          : Number of events / trials
      event_window (scalars)  : The time window surrounding each event for the rasters and perievent plots.
                                Express this with three values, in seconds for pre-event time, post-event time, 
                                and bin size (e.g., c(-5,5,.001)). The sample rate of your simulated data is inferred
                                from the bin size (e.g., bins of .001 seconds mean sample rate = 1000 Hz).
      base_window (scalars)   : Beginning and end of the baseline range in the pre-event period. Expressed in 
                                seconds and with two values. For instance, if base_window = c(-5,-2) and 
                                event_window = c(-5,5,.001)), then we do not expect a response (i.e., will treat as baseline)
                                in the period from -5 sec to -.2 sec pre-event.
      resp_window (scalars)   : Beginning and end of the response range in the post-event period. Expressed in 
                                seconds and with two values. For instance, if resp_window = c(.2,1) and 
                                event_window = c(-5,5,.001)), then we expect a response in the period from .2 sec to 1 sec post-event.
      stim_time (scalar)      : Time point relative to event_window where the stimulus/event takes place, in seconds. For instance,
                                if event_window = c(-5,5,.001), then a stim_time = 0 means that the onset of the stimulus/event is exactly
                                in the middle of this window, at t=0. If stim_time = -2, the stimulus occurs 3 seconds into the window.
                                If stim_time = 2.5, the neural response is modeled 7.5 seconds into the overall 10 s event_window.
      mean_FR (scalar)        : Mean baseline firing rate, in Hz. The Monte Carlo simulation will run until
                                it has generated data with this mean firing rate. Since simulated data is binary,
                                if you want the data to (closely) match this value, you must pick an integer. It follows
                                the following equation:  spike count = mean_FR / (sample rate / norm(base_window(1)-base_window(2)))
                                where the generated spike count will be the absolute number of spikes needed in base_window to
                                (approximately) match the requested mean_FR during that baseline period.
      amplitude (scalar)      : Amplitude of the neuron's response around baseline firing (i.e., mean_FR), in Hz,
                                to the simulated events. For instance, amplitude = 0 (total inhibition, 0 Hz),
                                amplitude = 1 (firing at baseline of mean_FR), amplitude = 2 (double the rate of baseline mean_FR).                            
      resp_peak (scalar)      : Temporal location of the neuron's response peak in relation to event, expressed
                                in seconds (e.g., resp_peak = .5, meaning 0.5 seconds after event onset).
                                Must lie within resp_window range.                          
      resp_perc (scalar)      : A neuron won't necessarily respond to each event every time. This
                                is the percentage of time (e.g. resp_perc = .6 for 60%) it will, 
                                in your simulated data (percentage expressed from -1 to 1).
                                If expressed as negative value (e.g. resp_perc = -.6), then the
                                remaining percentage of the time (in this case, 40%), there will
                                be a negative response (relative to baseline, but of course never below 0 Hz), in that
                                the neuron will show reduced firing.
      sigma (scalar)          : The width of the neuron's response (standard deviation of the gaussian), in
                                seconds (e.g., sigma = .3).
      noise_freq (ints)       : The frequency range of the sine wave that will be convolved with your data
                                as a noise signal. The first value is the frequency, the second is the
                                random modulation around that frequency (i.e. +/- that amount).
                                Express as a vector of two values in Hz (e.g., noise_freq = c(0,.1)).
      noise_amp (ints)        : The amplitude of the noise_freq sine wave, again expressed as two values.
                                The first value is the amplitude, the second the modulation around the amplitude, in Hz (e.g., c(.5,1)).
      refract (scalar)        : Refractory period of the neuron, in milliseconds. This prevents spikes occuring closely
                                together in time.
      modulator (scalar/char) : Denominator in some terms in the distribution function. Can be set to a value (e.g.,
                                modulator = 1e4) or to modulator = 'window' to match the length of the event_window.
      verbose (bool)          : Print updates to console (verbose=TRUE) or not (verbose=FALSE).
  "
  
  # Be nice
  message("
  Based on Michael Hill's expsim in the h-coefficient toolbox for Matlab.
  If you use this function, you must cite https://journals.physiology.org/doi/full/10.1152/jn.00595.2014
  
  ")
  
  # Initialize and adjust some variables
  samplerate = 1/event_window[3]                                          # Compute sample rate based on requested bin size
  amplitude = abs(amplitude)                                              # Ensure amplitude is positive  
  w_base = base_window * samplerate + abs(event_window[1]*samplerate)     # Adjust baseline period relative to start of event window
  w_resp = resp_window * samplerate + abs(event_window[1]*samplerate)     # Adjust response period relative to start of event window
  l_window = length(seq(event_window[1],event_window[2],event_window[3])) # Length of total peri-event window (n bins)
  sess_dur = duration * 60 * samplerate                                   # Convert based on sample rate to get total number of bins in session
  n_resp_events = round(abs(resp_perc) * n_events)                        # Number of events which elicit a response
  base_spikes = mean_FR * (w_base[2]-w_base[1]) / samplerate              # Spikes needed (per sec) to meet desired mean_FR in the baseline period within the event_window
  event_bins = ceiling(sess_dur / (n_events + 2))                         # Number of bins in each event period
  nonevent_bins = event_bins - l_window                                   # Number of bins in each non-event period (i.e., periods outside of event windows)
  y_lims = c(0,(1 + amplitude + amplitude*(noise_amp[1]+noise_amp[2])))   # Limits on the y-axis when plotting 
  freq_multiplier = 25                                                    # Multiply sampled noise_freq by a constant to introduce faster variation in the distribution over time
  nonevent_spikes = round(mean_FR * nonevent_bins / samplerate)           # Number of spikes needed in non-event period to approximate requested mean baseline firing rate
  
  # Any unusual parameter settings?
  if (sess_dur < n_events*l_window) { stop("The requested number of events will not fit in the session duration, based on event_window size.")}
  if (resp_peak < resp_window[1] | resp_peak > resp_window[2]) { stop("The requested resp_peak falls outside of requested resp_window.")}
  
  # Final spike train data frames
  rawdata = NULL
  perievents = array(NaN, c(n_events,l_window))
  periground = array(NaN, c(n_events,l_window))
  event_times = NULL
  
  # Create the artificial firing data
  for (i in 1:n_events) {   # For each event/stimulus presentation...
    
    # Noise frequency
    if (noise_freq[2] == 0) {
      nsfreq = noise_freq[1] * freq_multiplier   
    } else {
      nsfreq = noise_freq[1] + (runif(1) * 2 * noise_freq[2]) - noise_freq[2] * freq_multiplier # Randomize noise frequency
    }
    
    # Noise amplitude
    if (noise_amp[2] == 0) {
      nsamp = noise_amp[1]
    } else {
      nsamp = noise_amp[1] + (runif(1) * 2 * noise_amp[2]) - noise_amp[2] # Randomize noise amplitude
    }
    
    # Noise phase
    nsphase = runif(1) # Randomize noise phase
    
    if (verbose==TRUE) {
    message(paste0("Simulating neural event ",i,"/",n_events,", with nsfreq: ",round(nsfreq, digits=3),", nsamp: ",round(nsamp, digits=3),", nsphase: ",round(nsphase, digits=3)))
    }
      
    # First, simulate data in the period preceding the upcoming event window (i.e., a non-event period)
    sim = h.simulate.neuron_MCsim(signal=FALSE, xMin=1, xMax=nonevent_bins, n_trials=1, window=1:nonevent_bins, window_labels=c(1,nonevent_bins),
                                  n_spikes=nonevent_spikes, amplitude=amplitude, refract=refract, yMin=0, yMax=amplitude+1,
                                  y_lims=y_lims, samplerate=samplerate, plot=TRUE, modulator=1e4)
    nonevent_data = as.vector(sim[[1]]) # Simulated non-event data
    nonevent_grnd = sim[[2]] # Ground truth
    
    # Second, simulate period around the event (i.e., in the event window).
    # Is there a + response, - response, or no response?
    if (i <= n_resp_events) { # One of the + response events
      sim=h.simulate.neuron_MCsim(signal=TRUE, xMin=1, xMax=l_window, n_trials=1, window=1:l_window, window_labels=c(event_window[1],event_window[2]), stim_time = stim_time,
                                  n_spikes=base_spikes, amplitude=amplitude, refract=refract, yMin=0, yMax=amplitude+1, baseline_window=c(w_base[1],w_base[2]),
                                  y_lims=y_lims, samplerate=samplerate, sigma=sigma, resp_peak=resp_peak, plot=FALSE, modulator="window")
      event_data = na.omit(as.vector(sim[[1]])) # Simulated event data
      event_grnd = sim[[2]] # Ground truth
    } else if (resp_perc < 0) { # One of the - response events
      sim=h.simulate.neuron_MCsim(signal=TRUE, xMin=1, xMax=l_window, n_trials=1, window=1:l_window, window_labels=c(event_window[1],event_window[2]), stim_time = stim_time,
                                  n_spikes=base_spikes, amplitude=-amplitude, refract=refract, yMin=0, yMax=max(1,(-amplitude)+1), baseline_window=c(w_base[1],w_base[2]),
                                  y_lims=y_lims, samplerate=samplerate, sigma=sigma, resp_peak=resp_peak, plot=FALSE, modulator="window")
      event_data = na.omit(as.vector(sim[[1]]))
      event_grnd = sim[[2]]
    } else {  # One of the no-response events
      sim=h.simulate.neuron_MCsim(signal=FALSE, xMin=1, xMax=l_window, n_trials=1, window=1:l_window, window_labels=c(event_window[1],event_window[2]), stim_time = stim_time,
                                  n_spikes=base_spikes, amplitude=amplitude, refract=refract, yMin=0, yMax=amplitude+1, baseline_window=c(w_base[1],w_base[2]),
                                  y_lims=y_lims, samplerate=samplerate, sigma=sigma, resp_peak=resp_peak, plot=FALSE, modulator="window")
      event_data = na.omit(as.vector(sim[[1]]))
      event_grnd = sim[[2]]
    }
    
    # Transfer data over to final data frames
    # Also sort the spike data, where necessary. Spike times were drawn randomly across the various windows, so aren't temporally ordered yet.
    ith_block = i*event_bins
    rawdata = append(rawdata, sort(c( (nonevent_data+ith_block), (event_data+nonevent_bins+ith_block) )))
    perievents[i,1:length(((sort(event_data)+(window_labels[1]*samplerate))/samplerate))] = ((sort(event_data) + (window_labels[1]*samplerate))/samplerate)
    periground[i,] = event_grnd[1:l_window]
    event_times = append(event_times, c( ith_block + (abs(window_labels[1])*samplerate + stim_time*samplerate) ))                

  }
  
  # Some extra baseline firing at the beginning and ending of the session, to pad the data out
  for (b in c("start","end")) {
    
    # Noise frequency
    if (noise_freq[2] == 0) {
      nsfreq = noise_freq[1] * freq_multiplier   
    } else {
      nsfreq = noise_freq[1] + (runif(1) * 2 * noise_freq[2]) - noise_freq[2] * freq_multiplier # Randomize noise frequency
    }
    
    # Noise amplitude
    if (noise_amp[2] == 0) {
      nsamp = noise_amp[1]
    } else {
      nsamp = noise_amp[1] + (runif(1) * 2 * noise_amp[2]) - noise_amp[2] # Randomize noise amplitude
    }
    
    # Noise phase
    nsphase = runif(1) # Randomize noise phase
    
    if (verbose==TRUE) {
      message(paste0("Simulating padding at the ",b," of the session, with nsfreq: ",round(nsfreq, digits=3),", nsamp: ",round(nsamp, digits=3),", nsphase: ",round(nsphase, digits=3)))
    }
    
    # First, simulate data in the period preceding the upcoming event window (i.e., a non-event period)
    sim = h.simulate.neuron_MCsim(signal=FALSE, xMin=1, xMax=nonevent_bins, n_trials=1, window=1:nonevent_bins, window_labels=c(1,nonevent_bins),
                                  n_spikes=nonevent_spikes, amplitude=amplitude, refract=refract, yMin=0, yMax=amplitude+1,
                                  y_lims=y_lims, samplerate=samplerate, plot=TRUE, modulator=1e4)
    nonevent_data = as.vector(sim[[1]]) # Simulated non-event data
    nonevent_grnd = sim[[2]] # Ground truth
    
    if (b == "start") {
      rawdata = append(sort(nonevent_data), rawdata)
    } else {
      ith_block = (n_events+1)*event_bins
      rawdata = append(rawdata, sort(nonevent_data+ith_block))
    }
  }
  
  # Did we succeed in simulating data that approximates our desired parameters?
  if (verbose==TRUE) {
    message(paste0("
                   Simulation is done! The first spike occurs at ",round(min(rawdata)/samplerate,digits=3)," sec 
                   and the last spike at ",round(max(rawdata)/samplerate/60,digits=3)," mins into the simulated ",duration," min raw data.
                   The average firing rate across the entire session is ",round(length(rawdata)/(duration*60),digits=3)," Hz and you 
                   requested a mean_FR of ",mean_FR," Hz."))
  }
  
  # Finished!
  return(list(rawdata,perievents,periground,event_times))
  
}

