h.machinelearning.cost <- function(x, y, w, b, method = "descent") {

  "Compute the cost function for linear regression.
    
    Args:
      x           : Predictive values (rows = examples, columns = features)
      y           : Predicted, target values (single vector/column)
      w,b (scalar): Model parameters  𝑓𝑤,𝑏(𝑥)=𝑤𝑥+𝑏 (with either single or multiple predictors 𝑤*𝑥)
      method      : Method of computation ('descent' [batch gradient descent] or 'normal' [normal equation])
    
    Returns:
      total_cost (float): The cost of using w,b as the parameters for linear regression
                          to fit the data points y using weighted transformations of x"
  

  # What method of computation are we using?
  if (method == "descent") {
    
    # Single predictor/feature
    if (is.null(dim(x))) {
      
      # Setup
      n_examples = length(x)
      if (!is.numeric(x) | !is.numeric(y) ) { stop("For single feature cost computation, both x and y need to be numeric vectors.") }
      
      # Compute sum of all squared errors
      cost_sum = 0
      for (i in 1:n_examples) {
        f_wb_i = w * x[i] + b         # Product of ith example plus bias
        error = (f_wb_i - y[i]) ** 2  # Error of ith example's predicted score
        cost_sum = cost_sum + error   # Add ith error to total cost sum
      }
    }
    
    # Multiple predictors/features
    if (!is.null(dim(x))) {
      
      # Setup
      n_examples = dim(x)[1]
      if (!is.data.frame(x) | !is.numeric(y) ) { stop("For multiple feature cost computation, x should be a data frame and y should be a numeric vector.") }
      
      # Compute sum of all squared errors
      cost_sum = 0
      for (i in 1:n_examples) {
        f_wb_i = sum(x[i,] * w) + b   # Dot product of ith example plus bias
        error = (f_wb_i - y[i]) ** 2  # Error for ith example's predicted score
        cost_sum = cost_sum + error   # Add ith error to total cost sum
      }
    }
    
    # Compute average total cost and return
    total_cost = cost_sum / (2*n_examples)
    return(total_cost)
    
  }
  
  if (method == "normal") { "Computation with the normal equation hasn't been implemented yet. Sorry!" }
}
