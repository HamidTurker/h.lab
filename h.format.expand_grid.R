h.format.expand_grid <- function(x, method = "late") {
  
  " Takes a vector or list of vectors and returns an array containing one row
    for each combination of the items in the vector(s). If supplied a vector, all
    combinations between items are returned. If a list of vectors, each vector is
    treated as a separate factor with its own levels (so combinations are made across lists, not within).
  
    Args:
      x (vector)        : A vector or list of vectors
      method            : Which vectors are cycled through faster: 'early' or 'late' (for lists only)
  
    Returns:
      grid (array)      : An array with all combinations formatted based on chosen
                          method of cycling "
  
  # Source
  message("h.fomat.expand_grid :: v0.1: 2026 March 22")
  
  # Check args
  {
    # Invalid/No method chosen
    if (!method %in% c('early','late')) { 
      stop("Invalid method chosen: please choose 'early' or 'late'")
    }
  }
  
  
  # List of vectors
  if (is.list(x)) {
    
    # List info
    n_vectors = length(x) # Number of vectors
    n_items = array(NA, c(1,n_vectors)) # Number of items in each vector
    for (i in 1:n_vectors) {n_items[i] = length(x[[i]])} # Count items in each vector and update top row of n_items array
    l_finalgrid = prod(n_items[1,]) # Length of what the final grid will be (dim(grid)[1])
    
    # Final grid initialization
    grid = array(NA, c(l_finalgrid,n_vectors))
    
    # Populate grid based on requested method
    factor_count=0
    
    # Cycle through early vectors first
    if (method == "early") {
      
      # Expand grid
      for (v in 1:n_vectors) {
        factor_count = factor_count + 1
        if (factor_count == n_vectors) {
          grid[,factor_count] = rep(x[[v]], each=l_finalgrid/n_items[v])
        } else {
          grid[,factor_count] = rep(x[[v]], each=((l_finalgrid/prod(n_items[(1:n_vectors)[(v+1):n_vectors]]))/n_items[v]))
        }
      }
    }
    
    # Cycle through later vectors first
    if (method == "late") {
      
      # Expand grid
      for (v in 1:n_vectors) {
        factor_count = factor_count + 1
        if (factor_count == 1) {
          grid[,factor_count] = rep(x[[v]], each=l_finalgrid/n_items[v])
        } else {
          grid[,factor_count] = rep(x[[v]], each=((l_finalgrid/prod(n_items[(1:n_vectors)[1:(v-1)]]))/n_items[v]))
        }
      }
      
    }
  }
  
  # Vector
  if (!is.list(x)) {
    
    # Vector info
    n_items = length(x) # Number of items in the vector
    l_finalgrid = 0 # Length of what the final grid will be (dim(grid)[1])
    for (k in 1:n_items) {
      sz = choose(n_items,k)
      l_finalgrid = l_finalgrid + sz
    }
    
    # Final grid initialization
    grid = array(NA, c(l_finalgrid,n_items))
    
    # Populate grid
    count = 0
    for (k in 1:n_items) {
      chosen = utils::combn(x, k, simplify = TRUE)
      for (j in 1:dim(chosen)[2]) {
        count = count + 1
        grid[count,1:length(chosen[,j])] = chosen[,j]
      }
    }
  }
  
  # Finished
  return(grid)
}