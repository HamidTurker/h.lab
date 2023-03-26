h.calc.norm  <- function(x, k = NULL) {
  
  # NOT DONE
  
  "Calculate the Euclidian length (i.e. norm) of a vector without scaling.
  
    Args:
      x     : Vector
      k     : Desired k-norm (e.g., k = 2 returns 2 norm), must be k > 1"
  
  
  # Check if k is valid
  if (!length(k) == 1)  { message("k must be a single integer") }
  if (k < 2) { message("k must be > 1") }
  if (!k %% 1 == 0) { message("k must be a whole number") }
  
  # Compute k-norm
  x <- abs(x)
  if(k == Inf) { # Infinity-norm
    return(apply(x, 2, function(vec) max(vec)))
  } else { # k-norm
    return(apply(x, 2, function(vec) {
      max_vec <- max(vec)
      return(max_vec*(sum((vec/max_vec)^k))^(1/k))
    }))
  }
  
}