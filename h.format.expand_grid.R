h.format.expand_grid <- function(x, method = "late") {
  
  " Takes a vector or list of vectors and returns an array of containing one row
    for each combination of the items in the vector(s). If supplied a vector, all
    combinations between items are returned. If a list of vectors, each vector is
    treated as a separate factor with its own levels (so combinations are made across lists, not within).
  
    Args:
      x (vector)        : A vector or list of vectors
      method            : Which vectors are cycled through faster: 'early' or 'late'
  
    Returns:
      grid (array)      : An array with all combinations formatted based on chosen
                          method of cycling "
  
  # List of vectors
  if (is.list(x)) {
    
    # List info
    n_vectors = length(x) # Number of vectors
    n_items = array(NA, c(1,n_vectors)) # Number of items in each vector
    for (i in 1:n_vectors) {n_items[i] = length(x[[i]])} # Count items in each vector and update top row of n_items array
    len_finalgrid = prod(n_items[1,]) # Length of what the final grid will be (dim(grid)[1])
    
    # Final grid initialization
    grid = array(NA, c(len_finalgrid,n_vectors))
    
    # Populate grid based on requested method
    factor_count=0
    
    # Cycle through early vectors first
    if (method == "early") {
      
      # Expand grid
      for (v in 1:n_vectors) {
        factor_count = factor_count + 1
        if (factor_count == n_vectors) {
          grid[,factor_count] = rep(x[[v]], each=len_finalgrid/n_items[v])
        } else {
          grid[,factor_count] = rep(x[[v]], each=((len_finalgrid/prod(n_items[(1:n_vectors)[(v+1):n_vectors]]))/n_items[v]))
        }
      }

    # Cycle through later vectors first
     } else if (method == "late") {

      # Expand grid
      for (v in 1:n_vectors) {
        factor_count = factor_count + 1
        if (factor_count == 1) {
          grid[,factor_count] = rep(x[[v]], each=len_finalgrid/n_items[v])
        } else {
          grid[,factor_count] = rep(x[[v]], each=((len_finalgrid/prod(n_items[(1:n_vectors)[1:(v-1)]]))/n_items[v]))
        }
      }
    
    # Invalid/No method chosen
      } else { stop("Invalid method chosen: please choose 'early' or 'late'.")
    }
    
    # Finished
    return(grid)
  }
}