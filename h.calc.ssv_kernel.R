h.calc.ssv_kernel <- function(rawdata, tin = NULL, W = NULL, samplerate = 1e3, confidence = .95, win_func = 'Boxcar') {
  
  "
  Calculate the optimized kernel density estimate with a Gaussian kernel function
  where the bandwidth is locally adapted to the data.
  
  This function is based on ssvkernel.m, by Hideaki Shimazaki. If you use this
  function, you must cite Shimazaki & Shinomoto (2010):
  'Kernel Bandwidth Optimization in Spike Rate Estimation'
  http://dx.doi.org/10.1007/s10827-009-0180-4
  
    Args:
      rawdata (vector)    : Data vector over which the optimized kernel is computed.
      tin (vector)        : Points at which the estimation is computed.
      W (vector)          : A vector of kernel bandwidths to try. If provided, the
                            chosen bandwidth will be from your vector. If W is not
                            provided, the optimal bandwidth will be calculated
                            automatically, using a golden section search (default).
                            The bandwidths cannot be smaller than the sample rate.
      samplerate (scalar) : Sample rate of the data.
      confidence (scalar) : Desired bootstrapped confidence interval.
      win_func (char)     : Window function for smoothing: Gauss, Laplace, Caucy, or Boxcar (e.g., win_func = 'Boxcar')
      
    Return:
      y                   : Estimated density.
      t                   : Points at which estimation was computed.
      opt_W               : Optimal kernel bandwidth.
      gs                  : Stiffness constants of the variable bandwidth examined.
                            This is defined as the ratio of the optimal fixed bandwidth
                            to the length of a local interval in which a fixed-kernel
                            bandwidth optimization was performed.
      costs               : Cost functions of the stiffness constants.
      conf                : Bootstrapped confidence intervals.
      yb                  : Bootstrap samples.
      
      
      
  Optimization is based on the principle of minimizing expected L2 loss between the kernel
  and an unknown underlying density. It assumes that samples are drawn from the density function
  independently.
  
  The locally adaptive bandwidth is obtained by iteratively computing optimal fixed-size widths
  within local intervals. They are selected such that intervals that are gamma-times larger than
  the optimal bandwidths themselves. The parameter gamma is optimized by minizing the L2 risk estimate.
  "
  
  x = (forkern*(10^-(log(samplerate, base=10))))
  tin = seq(0.01,(((resp_dur*10)+padding)/samplerate),by=.01)
  
  ######################################################################################
  
  # Check args
  if (!is.null(W) & min(W) < (1/samplerate)) { stop("You requested a bandwidth (in W) that's below the sampling resolution of the data. This is not allowed.") }
  
  # Initialize
  M = 80                  # Number of bandwiths examined for optimization
  win_func = 'Boxcar'     # Window function (e.g., 'Gauss', 'Laplace', 'Cauchy', or 'Boxcar)
  n_bs = 100              # Number of bootstrap samples
  
  if (is.null(tin)) { 
    T = max(x) - min(x)
    sorted_diffs = sort(diff(sort(x)))
    dt_samp = sorted_diffs[sorted_diffs != 0][1] # Find lowest non-zero entry to be used as a delta time step
    delta = T/min(ceiling(T/dt_samp),samplerate) # Set up a time step delta that is based on either the dt_samp or provided sample rate (whatever is smallest)
    tin = seq(min(x), max(x), by=delta)
    x_ab = x[x >= min(tin) & x <= max(tin)]
    
    t = tin
  } else {
    T = max(tin) - min(tin)
    x_ab = x[x >= min(tin) & x <= max(tin)]
    sorted_diffs = sort(diff(sort(x_ab)))
    dt_samp = sorted_diffs[sorted_diffs != 0][1]
    
    if (dt_samp > min(diff(tin))) {
      delta = T/min(ceiling(T/dt_samp),samplerate)
      t = seq(min(tin), max(tin), by=delta)
    } else {
      t = tin
    }
  }
  dt = min(diff(t))
  
  # Create a fine-grained histogram
  y_hist = (h.calc.hist_count(x=x_ab, edges=(t-dt/2))$count)/dt
  L = length(y_hist)
  N = sum(y_hist*dt)
  
  # Compute local mean integrated squared error (MISE) and optimal bandwidths
  message("Computing local MISE (i.e., L2 risk) and optimal bandwiths..")
  
  # Window sizes
  start=h.math.ilogexp(max(5*dt))
  end=h.math.ilogexp(1*T)
  WIN = h.math.logexp(seq(start, end, by=(end-start)/M))
  
}


h.math.logexp <- function(x) {
  
  idx = x < 100
  y[idx] = log(1+exp(x[idx]))
  
  idx = x >= 100
  y[idx] = x[idx]
  
  return(y)
}

h.math.ilogexp <- function(x) {
  
  idx = x < 100
  y[idx] = log(exp(x[idx])-1)
  
  idx = x >= 100
  y[idx] = x[idx]
  
  return(y)
}




