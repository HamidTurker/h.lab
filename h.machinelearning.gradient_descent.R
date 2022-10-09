h.machinelearning.gradient_descent <- function(x, y, w_init, b_init, 
                                               alpha, num_iters,
                                               scale = FALSE,
                                               max_iters = 1e+05,
                                               conv_crit = 1e-100,
                                               div_crit = 1e+1000,
                                               search=FALSE, searchspace=3,
                                               cost.method = NULL, 
                                               gradient.method = NULL,
                                               verbose=TRUE, plot=TRUE) {

  " Performs gradient descent to fit w,b. Updates w,b by taking 
    num_iters gradient steps with learning rate alpha.
    
    Args:
      x                     : Predictive values
      y                     : Predicted, target values
      w_init,b_init (scalar): Initial values of model parameters  
      alpha (float)         : Learning rate
      num_iters (int)       : Number of iterations to run gradient descent
      scale (bool)          : Scale the features? This is recommended (default = TRUE)
      max_iters (int)       : Maximum number of iterations to run gradient descent
      conv_crit (float)     : Minimum cost criterion under which descent is considered converged.
                              So, if J(w,b) < conv_crit, descent is halted. Default = 1e-100.
      div_crit (float)      : Maximum cost criterion above which descent is considered diverged.
                              So, if J(w,b) > div_crit, descent is halted. Default = 1e+1000.
      search (bool)         : If TRUE, do a further search around the found w and b values
      searchspace (int)     : If search==TRUE, search all combination of w and b integer values
                              between w+/-searchspace*var(desc_hist of w) and b+/-searchspace*var(desc_hist of b)
                              (values > 4 can lead to long searches!)
      cost.method           : Choice of method to be called by h.machinelearning.cost [optional]
      gradient.method       : Choice of method to be called by h.machinelearning.gradient [optional]
      verbose (bool)        : If TRUE, print values of w and b to the console every 2000 iterations
      plot (bool)           : If TRUE, plot the cost function
      
    Returns:
      w (scalar)        : Updated value of parameter after running gradient descent
      b (scalar)        : Updated value of parameter after running gradient descent
      desc_hist (array) : History of cost (J), w, and b (values at each iteration)"
  
  # Flex parameters (can be adjusted, if you want)
  sigdigs=6 # Number of significant digits to be used when printing to console
  search_crit=20 # When plotting the cost surface, broad search computes variance around the w,b parameters. If this value is low (var < search_crit), the final plot may look odd. This parameter adjust the cutoff.
  col.surface=NULL # Color vector for surface plot (see options below, NULL = standard viridis scale)
  #col.surface=rev(c("#F2DC5D", "#F2A359", "#DB9065", "#A4031F", "#240B36"))
  #col.surface=rev(c("#F7AEF8", "#B388EB", "#8093F1", "#72DDF7", "#F4F4ED"))
  
  
  ### UNIVARIATE GRADIENT DESCENT ###
  if (is.null(dim(x)[1])) {
    
    # Initialize array for cost (J), w, and b at each iteration (for graphing)
    desc_hist = array(NA, c(num_iters,3)); colnames(desc_hist) = c("J","w","b")
    b = b_init
    w = w_init
    
    # Scale feature
    if (scale) {
    message("NOTE: Scaling requested, but there's only one feature. No scaling applied.
      You can scale the feature yourself, if you insist.")
    }
    
    # Gradient descent
    for (i in 1:num_iters) {
      
      # Calculate the gradient and update the parameters using gradient_function
      gradient = h.machinelearning.gradient(x, y, w, b)
      dj_dw=gradient[[1]]
      dj_db=gradient[[2]]
      
      # Update parameters
      w = w - alpha * dj_dw
      b = b - alpha * dj_db
      
      # Save cost J and parameters w & b at each iteration
      if (i < max_iters) { # prevent resource exhaustion 
        desc_hist[i,1] = h.machinelearning.cost(x, y, w, b) # Cost with w & b on this iteration
        desc_hist[i,2] = w # w on this iteration
        desc_hist[i,3] = b # b on this iteration
      } else { stop(paste0("Failed to converge. Search reached the maximum number of iterations (",max_iters,")!
                          Set max_iters to a higher value to continue trying with current parameters. Or, try
                          different parameters.")) }
      
      # Do we have convergence?
      if (desc_hist[i,1] < conv_crit) {
        
        # Remove all NA
        desc_hist<-na.omit(desc_hist)
        
        if (i == 1) {
          message(paste0("
    Gradient descent has converged after 1 iteration."))
        } else {
          message(paste0("
    Gradient descent has converged after ",i," iterations."))}
        break
      }
      
      # Do we have divergence?
      if (any(desc_hist[i,] > div_crit | desc_hist[i,] == -Inf)) {
        
        # Remove all NA
        desc_hist<-na.omit(desc_hist)
        
        if (i == 1) {
          stop(paste0("
    Gradient descent has diverged after 1 iteration. Does that sound right?
    w (",signif(desc_hist[i,2],digits=sigdigs),") and b (",signif(desc_hist[i,3],digits=sigdigs),"), with a cost (J_wb) of ",signif(desc_hist[i,1],digits=sigdigs),"."))
        } else {
          stop(paste0("
    Gradient descent has diverged after ",i," iterations.
    w (",signif(desc_hist[i,2],digits=sigdigs),") and b (",signif(desc_hist[i,3],digits=sigdigs),"), with a cost (J_wb) of ",signif(desc_hist[i,1],digits=sigdigs),"."))
        }
      }
      
      # Print cost every at intervals 10 times or as many iterations if < 10
      if (verbose) {
        if ( i == 1 | i %% ceiling(num_iters/5) == 0) {
          message(paste0("Iteration ",i," ::"))
          message(paste0("     dj_dw: ",signif(dj_dw,digits=sigdigs),", dj_db: ",signif(dj_db,digits=sigdigs)))
          message(paste0("     w: ",signif(w,digits=sigdigs),", b: ",signif(b,digits=sigdigs)))
          message(paste0("     Cost (J_wb): ",signif(desc_hist[i,1],digits=sigdigs)))
        } # Message end
      } # Verbose end
    } # Descent end
    
    # Print final results to console
    message(paste0("
                 
    Gradient descent found w (",signif(w,digits=sigdigs),") and b (",signif(b,digits=sigdigs),"), with a cost (J_wb) of ",signif(desc_hist[i,1],digits=sigdigs),"."))
    
    if (scale) {
      message("
    NOTE: This is another reminder that scaling was NOT applied!
          Scale the feature (x) yourself, if you really insist.")
    }
    
    # Compute broader cost surface
    if (search) {
      
      # Is searchspace too big?
      w_var=var(desc_hist[,2])
      b_var=var(desc_hist[,3])
      
      if (w_var < search_crit | b_var < search_crit) {
        if (w_var < search_crit) { w_var=50 }
        if (b_var < search_crit) { b_var=50 }
        message("
    Broad search requested. Note that there was low variance in the w,b history trace. Attempting to adjust plotting parameters, but it may look odd! Computing now..")
        
      } else {
        message("
    Broad search requested. Computing now..")
      }
      
      # Initialize search
      lowerbound_w=floor(w)-floor(w_var*searchspace); upperbound_w=floor(w)+floor(w_var*searchspace)
      lowerbound_b=floor(b)-floor(b_var*searchspace); upperbound_b=floor(b)+floor(b_var*searchspace)
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
      idx=which(grad_search$J==min(grad_search$J))
      
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
        plot(1:num_iters, desc_hist[,1], type="l",
             main="Cost over iterations", xlab="Iteration", ylab="Cost")
        
        # If we have high num_iters..
      } else {
        if (search) { # ..and we ran a broad search..
          par(mfrow=c(2,1))
          splitval=1000
          
          # Early search
          plot(1:splitval, desc_hist[1:splitval,1], type="l", 
               main="Early search", xlab="Iteration", ylab="Cost")
          
          # Late search
          plot(splitval:num_iters, desc_hist[splitval:num_iters,1], type="l", 
               main="Late search", xlab="Iteration", ylab="Cost")
          
          # 2d surface
          ggplot(grad_search, aes(w, b, z=J)) + 
            geom_tile() + 
            geom_contour_filled(colour="white")
          
          # 3d surface
          descent_trace=data.frame(cost=desc_hist[,1], w=desc_hist[,2], b=desc_hist[,3])
          search_dot=data.frame(w=grad_search[idx,1], b=grad_search[idx,2], cost=grad_search[idx,3])
          J=xtabs(J~w+b, data=grad_search)
          suppressWarnings(print(plot_ly(z=~J, type="surface", colors=col.surface, contours = list(
            z = list(show=TRUE,usecolormap=TRUE,highlightcolor="lightblue",project=list(z=TRUE)),
            y = list(show=TRUE,usecolormap=TRUE,highlightcolor="lightblue",project=list(y=TRUE)),
            x = list(show=TRUE,usecolormap=TRUE,highlightcolor="lightblue",project=list(x=TRUE)))) %>%
              add_trace(x=~b, y=~w, z=~cost, data=descent_trace,
                        type="scatter3d", mode="lines", 
                        line=list(color="red", width=4)) %>%
              add_trace(x=~b, y=~w, z=~cost, data=search_dot,
                        type="scatter3d", mode="markers",
                        marker=list(color="red", size=10)) %>%
              layout(title = 'Cost Surface with Descent Trace',
                     scene = list(xaxis=list(title="b"), yaxis=list(title="w"))) %>%
              hide_colorbar() %>% hide_legend()))
          
        } else { # ..and we didn't run a broad search..
          par(mfrow=c(2,1))
          splitval=1000
          
          # Early search
          plot(1:splitval, desc_hist[1:splitval,1], type="l",
               main="Early search", xlab="Iteration", ylab="Cost")
          
          # Late search
          plot(splitval:num_iters, desc_hist[splitval:num_iters,1], type="l",
               main="Late search", xlab="Iteration", ylab="Cost")
          
        } # End high iters, no search
      } # End high iters, with search
    } # End plot
    
    # Finished!
    return(list(w, b, desc_hist))
    
    
  } else {
    
    ### MULTIVARIATE GRADIENT DESCENT ###
    n_examples = dim(x)[1]
    n_features = dim(x)[2]
    
    # Initialize array for cost (J), w, and b at each iteration (for graphing)
    w.labels=NULL; for (i in 1:n_features) { w.labels = append(w.labels, paste0("w_",i)) }
    desc_hist = array(NA, c(num_iters,n_features+2)); colnames(desc_hist) = c("J",w.labels,"b")
    b = b_init
    w = w_init
    
    # Indices for last of all w's and the b (to improve readability in subsequent sections)
    idx_w = 2:(n_features+1)
    idx_b = (n_features+2)
    
    # Scale features
    if (scale) {
      message("Scaling features")
      for (i in 1:n_features) { 
      message("On x_",i,", unscaled |max-min| = ",diff(range(x[,i]))) }
      x = scale(x)
      for (i in 1:n_features) { 
      message("On x_",i,", the newly scaled |max-min| = ",diff(range(x[,i]))) }
      message("The range of the scaled features should now be similar. Is that right?")
    } else {
      message("NOTE: Features are not being scaled! Is that really what you want?")
      for (i in 1:n_features) { 
      message("On x_",i,", |max-min| = ",diff(range(x[,i]))) }
      message("If the range of the feature values are very different, scaling is strongly recommended!")
    }
    
    # Gradient descent
    for (i in 1:num_iters) {
      
      # Calculate the gradient and update the parameters using gradient_function
      gradient = h.machinelearning.gradient(x, y, w, b)
      dj_dw=gradient[[1]]
      dj_db=gradient[[2]]
      
      # Update parameters
      w = w - alpha * dj_dw
      b = b - alpha * dj_db
      
      # Save cost J and parameters w & b at each iteration
      if (i < max_iters) { # prevent resource exhaustion 
        desc_hist[i,1] = h.machinelearning.cost(x, y, w, b) # Cost with w & b on this iteration
        desc_hist[i,idx_w] = w # w on this iteration
        desc_hist[i,idx_b] = b # b on this iteration
      } else { stop(paste0("Failed to converge. Search reached the maximum number of iterations (",max_iters,")!
                          Set max_iters to a higher value to continue trying with current parameters. Or, try
                          different parameters.")) }
      
      # Do we have convergence?
      if (desc_hist[i,1] < conv_crit) {
        
        # Remove all NA
        desc_hist<-na.omit(desc_hist)
        
        if (i == 1) {
          message(paste0("
    Gradient descent has converged after 1 iteration."))
        } else {
          message(paste0("
    Gradient descent has converged after ",i," iterations."))}
        break
      }
      
      # Do we have divergence?
      if (any(desc_hist[i,] > div_crit | desc_hist[i,] == -Inf)) {
        
        # Remove all NA
        desc_hist<-na.omit(desc_hist)
        
        if (i == 1) {
          stop(paste0("
    Gradient descent has diverged after 1 iteration. Does that sound right?
    w (",signif(desc_hist[i,idx_w],digits=sigdigs),") and b (",signif(desc_hist[i,idx_b],digits=sigdigs),"), with a cost (J_wb) of ",signif(desc_hist[i,1],digits=sigdigs),"."))
        } else {
          stop(paste0("
    Gradient descent has diverged after ",i," iterations.
    w (",signif(desc_hist[i,idx_w],digits=sigdigs),") and b (",signif(desc_hist[i,idx_b],digits=sigdigs),"), with a cost (J_wb) of ",signif(desc_hist[i,1],digits=sigdigs),"."))
        }
      }
      
      # Print cost every at intervals 10 times or as many iterations if < 10
      if (verbose) {
        if ( i == 1 | i %% ceiling(num_iters/5) == 0) {
          message(paste0("Iteration ",i," ::"))
          message(paste0("     dj_dw: ",signif(dj_dw,digits=sigdigs)))
          message(paste0("     dj_db: ",signif(dj_db,digits=sigdigs)))
          message(paste0("     w: ",signif(w,digits=sigdigs)))
          message(paste0("     b: ",signif(b,digits=sigdigs)))
          message(paste0("     Cost (J_wb): ",signif(desc_hist[i,1],digits=sigdigs)))
        } # Message end
      } # Verbose end
    } # Descent end

    # Print final results to console
    message(paste0("
    
    Gradient descent found the following values:"))
    for (i in 1:n_features) {
    message(paste0("      w_",i,":  ",signif(w[i],digits=sigdigs)))
    }
    message(paste0("      b:    ",signif(b,digits=sigdigs)))
    message(paste0("      J:    ",signif(desc_hist[i,1],digits=sigdigs)))
    
    
    # Compute broader cost surface
    if (search) {
      
      # Search bounds
      if (n_features <= 5 ) { searchspace_multivar=10 } else { searchspace_multivar=3 }
      search_bounds=array(NA, c(2,n_features))
      for (i in 1:n_features) {
        search_bounds[1,i] = round(w[,i])-searchspace_multivar # lower
        search_bounds[2,i] = round(w[,i])+searchspace_multivar # upper
      }
      message(paste0("
    Broad search requested. Computing now with searchspace_multivar = ",searchspace_multivar,"."))
      
      # Initialize search
      #foo = array(NA, c((2*searchspace_multivar+1)*(searchspace_multivar^n_features),n_features))
      #for (i in 1:n_features) {
      #  foo[,i] = rep(search_bounds[1,i]:search_bounds[2,i], each=searchspace_multivar^(n_features-(i-1)))
      #}
      foo = array(NA, c(1+searchspace_multivar*2,n_features))
      for (i in 1:n_features) {
        foo[,i] = search_bounds[1,i]:search_bounds[2,i]
      }
      
      
      
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
      idx=which(grad_search$J==min(grad_search$J))
      
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
        plot(1:num_iters, desc_hist[,1], type="l",
             main="Cost over iterations", xlab="Iteration", ylab="Cost")
        
        # If we have high num_iters..
      } else {
        if (search) { # ..and we ran a broad search..
          par(mfrow=c(2,1))
          splitval=1000
          
          # Early search
          plot(1:splitval, desc_hist[1:splitval,1], type="l", 
               main="Early search", xlab="Iteration", ylab="Cost")
          
          # Late search
          plot(splitval:num_iters, desc_hist[splitval:num_iters,1], type="l", 
               main="Late search", xlab="Iteration", ylab="Cost")
          
          # 2d surface
          ggplot(grad_search, aes(w, b, z=J)) + 
            geom_tile() + 
            geom_contour_filled(colour="white")
          
          # 3d surface
          descent_trace=data.frame(cost=desc_hist[,1], w=desc_hist[,2], b=desc_hist[,3])
          search_dot=data.frame(w=grad_search[idx,1], b=grad_search[idx,2], cost=grad_search[idx,3])
          J=xtabs(J~w+b, data=grad_search)
          suppressWarnings(print(plot_ly(z=~J, type="surface", colors=col.surface, contours = list(
            z = list(show=TRUE,usecolormap=TRUE,highlightcolor="lightblue",project=list(z=TRUE)),
            y = list(show=TRUE,usecolormap=TRUE,highlightcolor="lightblue",project=list(y=TRUE)),
            x = list(show=TRUE,usecolormap=TRUE,highlightcolor="lightblue",project=list(x=TRUE)))) %>%
              add_trace(x=~b, y=~w, z=~cost, data=descent_trace,
                        type="scatter3d", mode="lines", 
                        line=list(color="red", width=4)) %>%
              add_trace(x=~b, y=~w, z=~cost, data=search_dot,
                        type="scatter3d", mode="markers",
                        marker=list(color="red", size=10)) %>%
              layout(title = 'Cost Surface with Descent Trace',
                     scene = list(xaxis=list(title="b"), yaxis=list(title="w"))) %>%
              hide_colorbar() %>% hide_legend()))
          
        } else { # ..and we didn't run a broad search..
          par(mfrow=c(2,1))
          splitval=1000
          
          # Early search
          plot(1:splitval, desc_hist[1:splitval,1], type="l",
               main="Early search", xlab="Iteration", ylab="Cost")
          
          # Late search
          plot(splitval:num_iters, desc_hist[splitval:num_iters,1], type="l",
               main="Late search", xlab="Iteration", ylab="Cost")
          
        } # End high iters, no search
      } # End high iters, with search
    } # End plot
    
    # Finished!
    return(list(w, b, desc_hist))
    
  } ### MULTIVARIATE GRADIENT DESCENT ###
}


  