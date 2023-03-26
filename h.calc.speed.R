h.calc.speed <- function(x_pos, y_pos, samplerate, resample = NULL, maxspeed = NULL) {
  
  # v0.1: 2022 July 26
  
  # Calculate the speed (e.g. running of rat) over the trajectory data
  # In addition to x and y positions, also provide the sample rate (in seconds)
  # and the maximum speed you find plausible (default=50 spatial units/second),
  # otherwise it will default to 1 SD above the mean speed
  # If a resample value is provided (in seconds), speed is averaged over that time period
  # and that value must be higher original sample rate (i.e. no faster sampling than original sample rate)
  
  # NB: any NaN's are removed
  
  ######################################################################################################
  ######################################################################################################
  
  # Convert sample rate from seconds to Hz
  samplerate = 1/samplerate
  
  # Compute heading and speed
  heading_x <- na.omit(diff(x_pos)); heading_y <- na.omit(diff(y_pos))
  speed <- sqrt(heading_x^2+heading_y^2)/(1/samplerate)
  
  # Correct for implausible extreme values
  if (is.null(maxspeed)) {
    crit <- mean(speed, na.rm=T)+sd(speed, na.rm=T)
    speed[speed > crit] <- mean(speed, na.rm=T)+sd(speed, na.rm=T)
  } else {
    speed[speed > maxspeed] <- maxspeed
  }
  speed <- c(0,speed) # Add leading zero to keep length of time series equal to original time series
  
  # Resample?
  if (!is.null(resample)) { # Average speed across new sample rate
    
    # Convert resample rate to Hz
    resample = 1/resample
    if (resample > samplerate) { stop("New sample rate cannot be lower (i.e. faster) than original sample rate") }
    
    factor = samplerate/resample
    new_timeseries = mean(speed[2:factor], na.rm=T) # Can skip leading zero
    for (i in 1:floor(length(speed)/factor)) {
      if (i < floor(length(speed)/factor)) {
        new_timeseries = append(new_timeseries, mean(speed[((factor*i)+1):((factor*i)+factor)]))
      } else {
        speed = append(new_timeseries, mean(speed[((factor*i)+1):length(speed)]))
      }
    }
  }
  
  return(speed)
}


