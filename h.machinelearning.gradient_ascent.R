h.machinelearning.gradient_ascent <- function(x, y, w_in, b_in, 
                                               alpha, num_iters,
                                               max_iters = 1e+05,
                                               search=TRUE, searchspace=3,
                                               cost.method = NULL, 
                                               gradient.method = NULL,
                                               verbose=TRUE, plot=TRUE) {
  
  " Performs gradient ascent to fit w,b. Updates w,b by taking 
    num_iters gradient steps with learning rate alpha.
    
    Args:
      x                 : Predictive values
      y                 : Predicted, target values
      w_in,b_in (scalar): Initial values of model parameters  
      alpha (float)     : Learning rate
      num_iters (int)   : Number of iterations to run gradient descent
      max_iters (int)   : Maximum number of iterations to run gradient descent
      search (bool)     : If TRUE, do a further search around the found w and b values
      searchspace (int) : If search==TRUE, search all combination of w and b integer values
                          between w+/-searchspace*var(history of w) and b+/-searchspace*var(history of b)
                          (values > 4 can lead to long searches!)
      cost.method       : Choice of method to be called by h.machinelearning.cost [optional]
      gradient.method   : Choice of method to be called by h.machinelearning.gradient [optional]
      verbose (bool)    : If TRUE, print values of w and b to the console every 2000 iterations
      plot (bool)       : If TRUE, plot the cost function
      
    Returns:
      w (scalar)        : Updated value of parameter after running gradient descent
      b (scalar)        : Updated value of parameter after running gradient descent
      history (array)   : History of cost (J), w, and b (values at each iteration)"
  
  # Initialize array for cost (J), w, and b at each iteration (for graphing)
  history = array(NA, c(num_iters,3)); colnames(history) = c("J","w","b")
  b = b_in
  w = w_in
  
  # Gradient ascent
  for (i in 1:num_iters) {
    
    # Calculate the gradient and update the parameters using gradient_function
    gradient = h.machinelearning.gradient(x, y, w, b)
    dj_dw=gradient[[1]]
    dj_db=gradient[[2]]
    
    # Update parameters
    w = w + alpha * dj_dw
    b = b + alpha * dj_db
    
    # Save cost J and parameters w & b at each iteration
    if (i < max_iters) { # prevent resource exhaustion 
      history[i,1] = h.machinelearning.cost(x, y, w, b) # Cost with w & b on this iteration
      history[i,2] = w # w on this iteration
      history[i,3] = b # b on this iteration
    }
    
    # Print cost every at intervals 10 times or as many iterations if < 10
    if (verbose) {
      sigdigs=6
      if ( i == 1 | i %% ceiling(num_iters/5) == 0) {
        message(paste0("Iteration ",i," ::"))
        message(paste0("     dj_dw: ",signif(dj_dw,digits=sigdigs),", dj_db: ",signif(dj_db,digits=sigdigs)))
        message(paste0("     w: ",signif(w,digits=sigdigs),", b: ",signif(b,digits=sigdigs)))
        message(paste0("     Cost (J_wb): ",signif(history[i,1],digits=sigdigs)))
      } # Message end
    } # Verbose end
  } # Descent end
  
  # Print final results to console
  message(paste0("
                 
  Gradient ascent found w (",signif(w,digits=sigdigs),") and b (",signif(b,digits=sigdigs),"), with a cost (J_wb) of ",signif(history[i,1],digits=sigdigs),"."))
  
  
  # Compute broader cost surface
  if (search) {
    message("
  Broad search requested. Computing now..")
    lowerbound_w=floor(w)-floor(var(history[,2])*searchspace); upperbound_w=floor(w)+floor(var(history[,2])*searchspace)
    lowerbound_b=floor(b)-floor(var(history[,3])*searchspace); upperbound_b=floor(b)+floor(var(history[,3])*searchspace)
    
    grad_search<-data.frame(
      w=rep(lowerbound_w:upperbound_w,each=length(lowerbound_b:upperbound_b)),
      b=rep(lowerbound_b:upperbound_b),
      J=NA
    )
    
    # Search this space..
    for (i in 1:dim(grad_search)[1]) {
      grad_search[i,3] = h.machinelearning.cost(x, y, grad_search[i,1], grad_search[i,2])
    }
    
    # Do the results (approximately) match?
    idx=which(grad_search$J==max(grad_search$J))
    
    message(paste0("
                 
    Broad search found w (",signif(grad_search[idx,1],digits=sigdigs),") and b (",signif(grad_search[idx,2],digits=sigdigs),"), with a cost (J_wb) of ",signif(grad_search[idx,3],digits=sigdigs),"."))
    
  } # Search end
  
  
  # Make cost surface matrix
  surface <- matrix(grad_search$J, nrow = length(lowerbound_w:upperbound_w), ncol = length(lowerbound_b:upperbound_b))
  mat_surface <- list(as.vector(lowerbound_w:upperbound_w),
                      as.vector(lowerbound_b:upperbound_b),
                      surface)
  
  
  # Plot cost?
  if (plot) {
    
    # With low num_iters, make one cost curve
    if (num_iters < 2000) {
      plot(1:num_iters, history[,1], type="l",
           main="Cost over iterations", xlab="Iteration", ylab="Cost")
      
      # If we have high num_iters..
    } else {
      if (search) { # ..and we ran a broad search..
        par(mfrow=c(2,1))
        splitval=1000
        
        # Early search
        plot(1:splitval, history[1:splitval,1], type="l", 
             main="Early search", xlab="Iteration", ylab="Cost")
        
        # Late search
        plot(splitval:num_iters, history[splitval:num_iters,1], type="l", 
             main="Late search", xlab="Iteration", ylab="Cost")
        
        # 2d surface
        ggplot(grad_search, aes(w, b, z=J)) + 
          geom_tile() + 
          geom_contour_filled(colour="white")
        
        # 3d surface
        descent_trace=data.frame(cost=history[,1], w=history[,2], b=history[,3])
        search_dot=data.frame(w=grad_search[idx,1], b=grad_search[idx,2], cost=grad_search[idx,3])
        J=xtabs(J~w+b, data=grad_search)
        plot_ly(z=~J, type="surface") %>%
          add_trace(x=~b, y=~w, z=~cost, data=descent_trace,
                    type="scatter3d", mode="lines", 
                    line=list(color="red", width=4)) %>%
          add_trace(x=~b, y=~w, z=~cost, data=search_dot,
                    type="scatter3d", mode="markers",
                    marker=list(color="white", size=10)) %>%
          layout(title = 'Cost Surface with Descent Trace',
                 scene = list(xaxis=list(title="b"), yaxis=list(title="w"))) %>%
          hide_colorbar() %>% hide_legend()
        
      } else { # ..and we didn't run a broad search..
        par(mfrow=c(2,1))
        splitval=1000
        
        # Early search
        plot(1:splitval, history[1:splitval,1], type="l",
             main="Early search", xlab="Iteration", ylab="Cost")
        
        # Late search
        plot(splitval:num_iters, history[splitval:num_iters,1], type="l",
             main="Late search", xlab="Iteration", ylab="Cost")
        
      } # End high iters, no search
    } # End high iters, with search
  } # End plot
  
  # Finished!
  return(list(w, b, history))
}