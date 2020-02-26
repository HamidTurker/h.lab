h.tools.model.parse <- function (model) {
  # Parse the model into its subcomponents. It expects '~' as the functional operator, allows algebraic operators like '+', '-', '*', '/', and '^n'.
  # To introduce a grouping/conditional factor, use '|'. To introduce multiple outcome variables or conditionals, use '&'.
  #
  # For example:
  # model = ACT & GRE ~ time * study + age | method & school
  # This model states that the multiple DVs (ACT and GRE) are a function of several regressors (time by study plus age).
  # And we want this conditioned on two factors (method and school).
  
  # VERSION ::: Feb 25 2020
  
  ##################################################################
  
  #lhs <- attr(terms(model), "variables")[2][[1]] # Left hand side of model (dependent variable)
  #rhs <- attr(terms(model), "variables")[3][[1]] # Right hand side of model (independent variable)
  
  #model = clean&raw&gender~time*age+hemi*study^2|rec&uni&stim
  #model = clean&raw&gender~time*age+hemi|rec&uni&stim
  model = clean~time*age+hemi|rec
  ##################################################################  
  
  # Initialize
  dv=NULL; iv=NULL; ops=NULL; cond=NULL
  
  # Get left and right hand sides of the model (lhs, rhs)
  form <- as.character(model); lhs <- form[2]; rhs <- form[3]
  
  # Further deconstruct the lhs (only '&' is allowed on the lhs)
  if (regexpr("&",lhs)[1] != -1) {  # If there are multiple outcome variables..
    n.dv <- length(attr(gregexpr("&",lhs)[[1]], "match.length"))+1 # Number of DVs
    dv <- array(1:n.dv, dim=c(n.dv,1)) # Initialize an array that we'll fill with the separate DVs
    
    # Now, let's parse out all the DVs
    start_position = 1 # We'll start at the left side of the string (index 1 in our string of the model), and update this as we move rightwards
    for (i in 2:n.dv-1) { # For every '&' found in the lhs of the model..
      if (i == 1) {
        end_position = gregexpr("&",lhs)[[1]][i]-2 # Find the location of the i-th '&'
        dv[i] = substr(lhs, start_position, end_position) # Parse out the i-th DV
        start_position  = gregexpr("&",lhs)[[1]][i]+2 } # Update the start_position to the end_position where we just left off
      
      if (i > 1) {
        end_position = gregexpr("&",lhs)[[1]][i]-2 # Find the location of the i-th '&'
        dv[i] = substr(lhs, start_position, end_position) # Parse out the i-th DV
        start_position  = gregexpr("&",lhs)[[1]][i]+2 } # Update the start_position to the end_position where we just left off
    }
    # The previous loop has now parsed out all but the last DV, let's not forget that one!
    dv[n.dv] = substr(lhs, start_position, nchar(lhs))
    
  } else { dv <- lhs } # If there is only one outcome variables..
  
  
  # Further deconstruct the rhs (common algebraic operators like '+', '*', '-', '/', '^n' and the conditional operator '|' and '&' are allowed)
  if (regexpr("\\|",rhs)[1] != -1) { # We'll start by checking if there are conditionals and parsing these out ("conditional hand side", chs)
    chs <- substr(rhs, regexpr("\\|",rhs)[1]+2, nchar(rhs))
    rhs <- substr(rhs, 1, regexpr("\\|",rhs)[1]-2)
    
    if (regexpr("&",chs)[1] != -1) {
      n.cond <- length(attr(gregexpr("&",lhs)[[1]], "match.length"))+1 # Number of conditionals
      cond <- array(1:n.dv, dim=c(n.dv,1)) # Initialize an array that we'll fill with the separate conditionals
      start_position = 1
      for (i in 2:n.cond-1) {
        if (i == 1) {
          end_position = gregexpr("&",chs)[[1]][i]-2 # Find the location of the i-th '&'
          cond[i] = substr(chs, start_position, end_position) # Parse out the i-th DV
          start_position  = gregexpr("&",chs)[[1]][i]+2 } # Update the start_position to the end_position where we just left off
        
        if (i > 1) {
          end_position = gregexpr("&",chs)[[1]][i]-2 # Find the location of the i-th '&'
          cond[i] = substr(chs, start_position, end_position) # Parse out the i-th DV
          start_position  = gregexpr("&",chs)[[1]][i]+2 } # Update the start_position to the end_position where we just left off
      }
    }
    cond = chs
    
  } else { cond = NULL } # If there aren't any conditionals, we can simply proceed
  
  
  if ((regexpr(" ",rhs)[1] != -1)) { # If there are multiple regressors/predictors/IVs..
    
    if (length(attr(gregexpr(" ",rhs)[[1]],'match.length')) %% 2 == 0) {
      n.iv <- length(attr(gregexpr(" ",rhs)[[1]],'match.length'))-2 # Even number of IVs
      iv <- array(1:n.iv, dim=c(n.iv,1))
      start_position = 1; j = 0
      for (i in 2:n.iv-1) {
        if (i == 1) {
          end_position = gregexpr(" ",rhs)[[1]][i]-1 # Find the location of the i-th ' '
          iv[i] = substr(rhs, start_position, end_position) } # Parse out the i-th DV
        
        if (i > 1) {
          start_position = gregexpr(" ",rhs)[[1]][i+j]+1 # Update the start_position to the end_position where we just left off
          j=j+1
          end_position = gregexpr(" ",rhs)[[1]][i+j]-1 # Find the location of the i-th ' '
          iv[i] = substr(rhs, start_position, end_position) } # Parse out the i-th DV
      }
      iv[n.iv] = substr(rhs, gregexpr(" ",rhs)[[1]][n.iv+2]+1, nchar(rhs))
    } else {
      n.iv <- length(attr(gregexpr(" ",rhs)[[1]],'match.length'))-1 # Odd number of IVs
      iv <- array(1:n.iv, dim=c(n.iv,1))
      start_position = 1
      for (i in 2:n.iv-1) {
        if (i == 1) {
          end_position = gregexpr(" ",rhs)[[1]][i]-1 # Find the location of the i-th ' '
          iv[i] = substr(rhs, start_position, end_position) } # Parse out the i-th DV
        
        if (i > 1) {
          start_position = gregexpr(" ",rhs)[[1]][i]+1 # Update the start_position to the end_position where we just left off
          end_position = gregexpr(" ",rhs)[[1]][i+1]-1 # Find the location of the i-th ' '
          iv[i] = substr(rhs, start_position, end_position) } # Parse out the i-th DV
      }
      iv[n.iv] = substr(rhs, gregexpr(" ",rhs)[[1]][n.iv+1]+1, nchar(rhs))
    }
    
    n.ops <- n.iv-1 # Number of operators
    ops <- array(1:n.ops, dim=c(n.ops,1))
    
    entry=1
    for (i in seq(1,n.ops*2,2)) {
      start_position = gregexpr(" ",rhs)[[1]][i]+1 
      end_position = gregexpr(" ",rhs)[[1]][i+1]-1 # Find the location of the i-th ' '
      ops[entry] = substr(rhs, start_position, end_position) # Parse out the i-th operator
      entry = entry+1
    }
  } else { iv <- rhs } # If there is only one regressor..
  
  # Parsed model
  return(list(dv,iv,ops,cond))
}