h.calc.hist_count <- function(x, edges) {
  
  "
  Histogram count, behaves like MATLAB's / pracma's 'histc'. It counts the number of
  elements in the vector/matrix x that fall between the provided edges. For matrices,
  it returns a matrix of column-wise histogram counts.
  "
  
  # Check args
  if (is.unsorted(edges)) { stop("Argument 'edges' must be a monotonically non-decreasing vector.") }
  
  # Initialize
  edges <- c(edges)
  n <- length(edges)
  bins <- numeric(length(x))
  
  # If only one edge provided..
  if (n == 1) { 
    if (is.matrix(x)) { dim(bins) <- c(n, ncol(x)) }
    return(list(count = 0, bins = bins))
  }
  
  # If there's more than 1 edge..
  # Is x a vector or matrix?
  if (is.vector(x)) {       # If it's a vector..
    count <- numeric(n)
    for (i in 1:(n-1)) {
      idx <- edges[i] <= x & x < edges[i+1]
      count[i] <- sum(idx)
      bins[idx] <- i
    }
    idx <- x == edges[n]
    count[n] <- sum(idx)
    bins[idx] <- n
    
  } else if (is.matrix(x)) { # If it's a matrix..
    count <- matrix(0, n, ncol(x))
    for (i in 1:(n-1)) {
      idx <- edges[i] <= x & x < edges[i+1]
      count[i,] <- apply(idx, 2, sum)
      bins[idx] <- i
    }
    idx <- x == edges[n]
    count[n,] <- apply(idx, 2, sum)
    bins[idx] <- n
    
  } else {
    stop("Argument 'x' must be a numeric vector or matrix.")
  }
  
  dim(bins) <- dim(x)
  return(list(count = count, bins = bins))
}