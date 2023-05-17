# Source
message("h.simulate.neuron_MCsim :: v0.1: 2022 Dec 1")

# Function
h.simulate.neuron_MCsim <- function(signal=TRUE, xMin=1, xMax=10000, n_trials=1,
                                    window=1:10001, window_labels=c(-5,5), n_spikes=100,
                                    baseline_window=c(0,4800), refract=3, yMin=0, yMax=10, resp_peak=.5,
                                    y_lims=NULL, sigma=.3, amplitude=2, stim_time=0,
                                    samplerate=1e3, plot=TRUE, modulator=1e4) {
  
  "
  Monte Carlo simulation of a single neuron's spiking activity.
  
  Based on Michael Hill's expsim in the h-coefficient toolbox for Matlab.
  If you use this function, you must cite https://journals.physiology.org/doi/full/10.1152/jn.00595.2014
  
    Args:
      signal (bool)                 : Do you want to simulate a neuron responding to an actual signal (TRUE) or simulate baseline firing (FALSE)?
      xMin (num)                    :
      xMax (num)                    :
      n_trials (num)                : The number of trials you want to simulate with the same parameters.
      window (vec of nums)          : Vector of numeric labels for the temporal bins.
      window_labels (vec of nums)   : The labels for simulated time window (e.g., window_labels = c(-5,5) simulates a neuron from t=-5 s to t=5 seconds of firing around a
                                      simulated event centered on t=0).
      n_spikes (num)                : Number of simulated spikes to be distributed across the window.
      baseline_window (vec of nums) :
      refract (scalar)
      yMin (num)
      yMax (num)
      resp_peak (num)
      y_lims (num)
      sigma (scalar)
      amplitude (scalar)
      stim_time (num)
      samplerate (num)
      plot (bool)
      modulator (num)
  "
  
  # Set basic parameters
  if (modulator == "window") { modulator = length(window) }
  dist_line = 2
  dist_col = "blue"
  spike_col = "red"
  sleep = .5
  
  ##### SIMULATION WITH SIGNAL
  if (signal == TRUE) { # If we want to model a neuron with signal
    message("NOTE: You requested a simulation WITH signal. All possible arguments to this function will be used:
            xMin, xMax, n_trials, window, window_labels, n_spikes, baseline_window, stim_time, resp_peak,
            sigma, amplitude, refract, yMin, yMax, y_lims, samplerate, plot, modulator.")
    
    # Initialize
    data=array(NaN, c(n_trials,xMax))
    
    # Set up the noise distribution to be sampled from
    resp_peak = abs(window_labels[1])*samplerate + stim_time*samplerate + resp_peak*samplerate
    sigma = sigma*samplerate
    
    # Let's sample
    for (t in 1:n_trials) {
      
      # Visualize the MC? Set up the basic plot to show the noise wave
      delta = length(window) / modulator
      x = seq(xMin,xMax,by=delta)
      fx = (((exp(-(x-resp_peak)^2/((2*sigma)^2)))*amplitude)+1) + sin(((x+(nsphase*modulator))*((nsfreq*pi)/modulator)))*(nsamp)        
      if (plot==TRUE) {
        plot(x, fx, xlim=c(xMin,xMax), ylim=y_lims, type="l", lwd=dist_line, col=dist_col,
             xlab="Signal Bins", ylab="Signal Distribution", main="SIGNAL Monte Carlo simulation in progress...")
        abline(v = abs(window_labels[1])*samplerate, col = "black", lwd=3)
      }
      
      # Vectors to hold our drawn x and y values
      x_values = 0; y_values = 0;
      
      # Monte Carlo
      while (length(x_values[x_values > baseline_window[1] & x_values < baseline_window[2]]) < n_spikes) { # Generate spikes until desired number is reached to meet the requested n_spikes in the baseline_window
        
        # Generate an x and y within our bounds
        x = runif(1, min=xMin, max=xMax)
        y = runif(1, min=yMin, max=yMax)
        fx_i = (((exp(-(x-resp_peak)^2/((2*sigma)^2)))*amplitude)+1) + sin(((x+(nsphase*modulator))*((nsfreq*pi)/modulator)))*(nsamp)
        
        if ((y <= fx_i) & suppressWarnings(abs(x-max(x_values[x_values < x]))) > refract) { # Is the randomly drawn y value below or on the noise curve and not within another spike's refractory period? If so, proceed..
          
          # Append the valid drawn x and y values to our x_values and y_values vectors (if first valid drawing, just swap out the 0)
          if (x_values[1] == 0 & length(x_values) == 1) { x_values = x; y_values = y } else {
            x_values = append(x_values, x)
            y_values = append(y_values, y)
          }
          
          if (plot==TRUE) {
            points(x_values, y_values, pch=20, col='grey') # Plot drawn points (previous ones in black, most recent in red)
            points(x, y, pch=20, col=spike_col)
            text(x, 0, "'", col = 1, cex=2)
            Sys.sleep(sleep)
          }
        }
      }
      # Final x_values (i.e., bins with spikes)
      data[t,1:length(x_values)] = x_values
    }
    # Done!
    return(list(data,fx))
    
    
    
  ##### SIMULATION WITHOUT SIGNAL (BASELINE/NOISE FIRING)  
  } else { # If we want to model a neuron without signal (i.e., just baseline/noise)
    message("NOTE: You requested a simulation WITHOUT signal (i.e., just baseline/noise). Only the following arguments will be used:
            xMin, xMax, n_trials, window, window_labels, n_spikes,  amplitude, refract, yMin, yMax, y_lims,
            samplerate, plot, modulator.")
    
    # Initialize
    data=array(NaN, c(n_trials,ceiling(n_spikes)))
    
    # Let's sample
    for (t in 1:n_trials) {
      
      # Set up the noise distribution to be sampled from
      delta = (xMax - xMin) / modulator
      x = seq(xMin,xMax,by=delta)
      fx = 1 + sin( ((x+(nsphase*modulator)) * ((nsfreq*pi)/modulator)) ) * (nsamp)
      fx[fx < 0] = 0
      
      # Visualize the MC? Set up the basic plot to show the noise distribution
      if (plot==TRUE) {
        plot(x, fx, xlim=c(xMin,xMax), ylim=y_lims, type="l", lwd=dist_line, col=dist_col,
             xlab="Noise Bins", ylab="Noise Distribution", main="BASELINE Monte Carlo simulation in progress...")
      }
      
      # Vectors to hold our drawn x and y values
      x_values = 0; y_values = 0;
      
      # Monte Carlo
      while (length(x_values) < n_spikes) { # Generate spikes until desired number of baseline/noise spikes is reached
        
        # Generate an x and y within our bounds
        x = runif(1, min=xMin, max=xMax)
        y = runif(1, min=yMin, max=yMax)
        fx_i = 1 + sin( ((x+(nsphase*modulator)) * ((nsfreq*pi)/modulator)) ) * (nsamp)
        
        if ((y <= fx_i) & suppressWarnings(abs(x-max(x_values[x_values < x]))) > refract) { # Is the randomly drawn y value below or on the noise curve and not within another spike's refractory period? If so, proceed..
          
          # Append the valid drawn x and y values to our x_values and y_values vectors (if first valid drawing, just swap out the 0)
          if (x_values[1] == 0 & length(x_values) == 1) { x_values = x; y_values = y } else {
            x_values = append(x_values, x)
            y_values = append(y_values, y)
          }
          
          if (plot==TRUE) {
            points(x_values, y_values, pch=20, col='grey') # Plot drawn points (previous ones in black, most recent in red)
            points(x, y, pch=20, col=spike_col)
            text(x, 0, "'", col = 1, cex=2)
            #Sys.sleep(.1)
          }
        }
      }
      # Final x_values (i.e., bins with spikes)
      data[t,] = x_values
    }
    # Done!
    return(list(data,fx))
  }
}

