# Source
message("h.graph.pop_perievent :: v0.1: 2023 Mar 18")

# Function
h.graph.pop_perievent <- function(popA, popB = NULL, sort = NULL, samplerate = .1, popA_idx = NULL, popB_idx = NULL,
                                   window = c(-5,5), normalize = TRUE, smooth = TRUE, kernel_bw = 10, kernel_type = "normal",
                                   contrast = FALSE, nan2zero = TRUE) {
  
  "
  Make a population level array for plotting. The array can be sorted by a peak, windowed-average, and/or
  contrasted to another population plot.
  
  
    Args:
      popA (array)        : Array with peri-event data (rows are neurons, columns are time bins)
      popB (array)        : Optional second array with peri-event data, if you want to subtract arrays
      sort (char,num,vec) : How do you want the population to be sorted?
                            Can provide the following character arguments: 
                            'max top left', 'max top right', 'max bottom left', 'max bottom right', 
                            'min top left', 'min top right', 'min bottom left', 'min bottom right'.
                            For instance, 'min bottom right' puts the neuron with the lowest activity occuring
                            the furthest to the right in time on the bottom row.
                            You can also provide a character and two numbers, e.g. c('max',0,5), and the population
                            will be sorted by the maximum average value between 0 and 5 seconds on the peri-event
                            histogram. The numeric window must be in seconds and, naturally, fall within the overall
                            range of the peri-event histogram.
                            You can also provide your own vector of neuron numbers to sort the population by. This
                            vector must be numeric.
      samplerate (num)    : Sample rate of the time bins (in seconds)
      popA_idx (vec)      : Vector of the current neuron order that popA is in (otherwise, first row is neuron 1,
                            second neuron is neuron 2, etc.)
      popB_idx (vec)      : Vector of the current neuron order that popB is in
      window (num)        : Window of the peri-event histogram, e.g. c(-5,5) with samplerate=.1 would mean that the
                            columns of the data represent, from left to right, -5.0 s, -4.9 s, -4.8 s, etc.
      normalize (bool)    : Do you want to normalize the firing rate (TRUE) or not (FALSE)?
      kernel_bw (scalar)  : If smooth = TRUE, what's the bandwidth of the smoothing kernel?
      kernel_type (char)  : If smooth = TRUE, what's the type of smoothing kernel you want?
      contrast (bool)     : Do you want to subtract popB from popA (TRUE) or not (FALSE)? If TRUE, normalization is
                            recommended.
      nan2zero (bool)     : When normalize = TRUE, neurons with no firing will turn to NANs. If nan2zero = TRUE, this
                            replaces those NANs with 0s.
                            
  "
  
  # Check args
  if (!is.null(popB)) { if (!dim(popA)[1] == dim(popB)[1]) { stop("The popA and popB data do not have the same number of neurons.") } }
  if (!is.null(popB) & (!contrast)) { message("You provided two populations, but did not ask for a contrast. Is that correct?") }
  
  # Setup
  n_cells = dim(popA)[1]
  time_win = seq(window[1],window[2],by=samplerate)
  l_window = length(time_win)
  
  # Is there a separate population index vector provided?
  if (!is.null(popA_idx)) {
    for (i in 1:n_cells) { hold = popA[popA_idx[i],] }
    popA = hold
    message("Note that the data will be returned sorted according to the vector you provided, not the original order.")
  }
  if (!is.null(popB_idx)) {
    for (i in 1:n_cells) { hold = popB[popB_idx[i],] }
    popB = hold
  }
  
  # Smooth the firing rate within each cell
  if (smooth) {
    for (i in 1:n_cells) { popA[i,] = ksmooth(1:l_window, popA[i,], kernel=kernel_type, bandwidth=kernel_bw)$y }
    
    # Also for popB, if present
    if (!is.null(popB)) {
      for (i in 1:n_cells) { popB[i,] = ksmooth(1:l_window, popB[i,], kernel=kernel_type, bandwidth=kernel_bw)$y  }
    }
  }
  
  # Normalize firing rate within each cell
  if (normalize) {
    for (i in 1:n_cells) { popA[i,] = (popA[i,]-min(popA[i,])) / (max(popA[i,])-min(popA[i,])) }
    
    # Also for popB, if present
    if (!is.null(popB)) {
      for (i in 1:n_cells) { popB[i,] = (popB[i,]-min(popB[i,])) / (max(popB[i,])-min(popB[i,])) }
    }
  }
  
  # Adjust any introduced NANs to 0s?
  if (nan2zero) { 
    popA[is.na(popA)] = 0
    
    # Also for popB, if present
    if (!is.null(popB)) { popB[is.na(popB)] = 0 } 
  }
  
  # Contrast population frames
  if (contrast) {
    if (is.null(popB)) { stop("You asked for a contrast, but didn't provide a second population (popB).") }
    
    popA = popA-popB
  }
  
  # Sorting
  if (!is.null(sort)) {
    
    sort_idx = NULL
    sort_check = F
    
    # Max or min
    if (length(sort) == 1 & class(sort) == 'character') {
      if (sort == 'max top left' | sort == 'max top right' | sort == 'max bottom left' | sort == 'max bottom right') {
        sort_check = T # Sort flag
        for (i in 1:n_cells) { # Find the (first) bin with the min for each neuron
          if (max(popA[i,])==0) { sort_idx = append(sort_idx, values = 0) } else { sort_idx = append(sort_idx, values = which(popA[i,]==max(popA[i,]))[1]) }
        }
      }
      if (sort == 'min top left' | sort == 'min top right' | sort == 'min bottom left' | sort == 'min bottom right') {
        sort_check = T # Sort flag
        for (i in 1:n_cells) {  # Find the (first) bin with the min for each neuron
          if (max(popA[i,])==0) { sort_idx = append(sort_idx, values = 0) } else { sort_idx = append(sort_idx, values = which(popA[i,]==min(popA[i,]))[1]) }
        }
      }
      
      # Sorting method
      sort_frame = cbind(1:n_cells, sort_idx) # Match to the cell number
      if (sort == 'max top left') { sort_frame = sort_frame[order(sort_frame[,2], decreasing = T),] }
      if (sort == 'max top right') { sort_frame = sort_frame[order(sort_frame[,2], decreasing = F),] }
      if (sort == 'max bottom left') { sort_frame = sort_frame[order(sort_frame[,2], decreasing = T),]; sort_frame[,1] = rev(sort_frame[,1]) }
      if (sort == 'max bottom right') { sort_frame = sort_frame[order(sort_frame[,2], decreasing = F),]; sort_frame[,1] = rev(sort_frame[,1]) }
      if (sort == 'min top left') { sort_frame = sort_frame[order(sort_frame[,2], decreasing = T),] }
      if (sort == 'min top right') { sort_frame = sort_frame[order(sort_frame[,2], decreasing = F),] }
      if (sort == 'min bottom left') { sort_frame = sort_frame[order(sort_frame[,2], decreasing = T),]; sort_frame[,1] = rev(sort_frame[,1]) }
      if (sort == 'min bottom right') { sort_frame = sort_frame[order(sort_frame[,2], decreasing = F),]; sort_frame[,1] = rev(sort_frame[,1]) }
    }
    
    # Windowed average
    if (length(sort) == 3 & class(sort) == 'character') {
      
      # The indices of the bins that we'll be averaging between
      from = which(time_win == as.numeric(sort[2]))
      to = which(time_win == as.numeric(sort[3]))
      
      # Vector of averages per neuron
      for (i in 1:n_cells) { sort_idx = append(sort_idx, mean(popA[i,from:to])) }
      sort_frame = cbind(1:n_cells, sort_idx) # Match to the cell number
      
      # Sorting method
      if (sort[1] == "max") {
        sort_frame = sort_frame[order(sort_frame[,2], decreasing = F),]
      } else if (sort[1] == "min") {
        sort_frame = sort_frame[order(sort_frame[,2], decreasing = T),]
      } else { stop("First argument must be 'max' or 'min'.") }
      
      # Sort flag
      sort_check = T
    }
    
    # Sort by provided vector
    if (length(sort) == n_cells) {
      sort_frame = cbind(sort, 1:n_cells) # Match to the cell number
      
      # Sort flag
      sort_check = T
    }
    
    # Sort flag
    if (!sort_check) { stop("You didn't provide a valid sorting method") }
    
    ### Implement sorting
    sorted_pop = NULL
    for (i in 1:n_cells) { sorted_pop = rbind(sorted_pop, popA[sort_frame[i,1],]) }
    
  } else { sorted_pop = popA }
  
  # Finished!
  return(list(sorted_pop,sort_frame))
  
}