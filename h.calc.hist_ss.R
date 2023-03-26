h.calc.hist_ss <- function(x, n = 100, plotting = FALSE) {
  
  "
  Histogram binsize optimizing, behaves as pracma's 'histss'. Calculate the
  bin sizes of histograms that optimizes them in a way to best displays
  the underlying spike rate.
  
  Based on pracma's implementation of the method outlined in:
  Shimazaki H. and S. Shinomoto. A method for selecting the bin size of a time histogram.
  Neural Computation (2007) Vol. 19(6), 1503-1527
  
  Args:
    x (vector/matrix)   : Vector or matrix of data to be binned.
    n (int)             : Maximum number of bins.
    plotting (bool)     : Plot the results (plotting = TRUE) or not (plotting = FALSE)
  "
  
  # Check args
  if (!is.numeric(x) | !is.numeric(n)) { stop("Ensure that both args 'x' and 'n' are numeric.") }
  if (length(n) > 1 || n < 2 || floor(n) != ceiling(n)) { stop("Argument 'n' must be an integer greater than 1.") }
    
  # Initialize
  x <- c(x)
  D <- C <- numeric(n-1)
   
  for (i in 1:(n-1)) {
    D[i] <- diff(range(x)) / (i+1)
    
    E    <- seq(min(x), max(x), length.out = i+1)
    hp   <- hist(x, breaks = E, plot = FALSE)
    ki   <- hp$counts
    k    <- mean(ki)
    v    <- sum((ki-k)^2) / (i+1)
    
    # Cost function
    C[i] <- (2*k - v) / D[i]^2
  }
  
  idx  <- which.min(C)
  optD <- D[idx]
  E    <- seq(min(x), max(x), length = idx+1)
  h    <- hist(x, breaks = E, plot = plotting)    # rug(x)
  
  if (plotting) { invisible(h) } else { return(h) }
}