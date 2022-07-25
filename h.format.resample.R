h.format.resample <- function(timeseries = NULL, by = NULL, old_samplerate = 1000, new_samplerate = 100) {
  
  old_samplerate = 1000
  new_samplerate = 100
  factor = old_samplerate/new_samplerate
  
  timeseries = seq(1,1000,1)
  
  length(timeseries)
  n_timepoints = length(timeseries)/factor
  
  new_timeseries = timeseries[1]
  for (i in 1:new_samplerate-1) {
    new_timeseries = append(new_timeseries, timeseries[i*factor])
  }
  
}
