h.calc.firingrate <- function(spiketrain, samplerate, n_samples, bin = NULL) {
  
  # v0.1: 2022 July 27
  
  # Calculate the firing rate of a spike train (vector of time stamps at which a spike occurred)
  # Provide the sample rate of the data (in Hz), total number of samples,
  # and the desired bin size (in Hz) over which firing rate is to be calculated
  
  ######################################################################################################
  ######################################################################################################
  
  # Convert sample rates from seconds to Hz
  samplerate = 1/samplerate
  bin = 1/bin
  
  factor = samplerate/bin
  firingrate <- array(0, dim=ceiling(n_samples/factor))
  
  for (i in 0:(length(firingrate)-1)) {
    firingrate[i+1] <- length(spiketrain[spiketrain >= i*(1/bin) & spiketrain < i*(1/bin)+(1/bin)])
  }
  
  # Hz
  firingrate = firingrate/bin
  
  return(firingrate)
}