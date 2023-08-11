# Source
message("h.machinelearning.gradient :: v0.1: 2023 Aug 8")

# Function
h.machinelearning.gradient <- function(x, y, w, b, method = NULL, model = "linear") {

  "Compute the gradient for linear regression.
  
    Args:
      x               : Predictive values (rows = examples, columns = features)
      y               : Predicted, target values (single vector/column)
      w,b (scalar)    : Model parameters  𝑓𝑤,𝑏(𝑥)=𝑤𝑥+𝑏 (with either single or multiple predictors 𝑤*𝑥)
      method (char)   : Method of computation
      model (char)    : Regression model, can be 'linear', 'logistic'
  
    Returns:
      dj_dw (scalar)  : The gradient of the cost w.r.t. the parameters w
      dj_db (scalar)  : The gradient of the cost w.r.t. the parameter b"
  
  
  # Single predictor/feature
  if (is.null(dim(x))) {
    
    # Setup
    n_examples = length(x)
    dj_dw = 0
    dj_db = 0
    
    # Compute gradient
    for (i in 1:n_examples) {
      
      # Regression model with provided parameters
      if (model == "linear") { f_wb_i = w * x[i] + b }
      if (model == "logistic") { f_wb_i = 1 / (1 + exp(-(w * x[i] + b))) }
      
      dj_dw_i = (f_wb_i - y[i]) * x[i]  # Error * ith feature (part of the partial derivative)
      dj_db_i = f_wb_i - y[i]           # Error for b
      dj_dw = dj_dw + dj_dw_i           # Update w and b
      dj_db = dj_db + dj_db_i
    }
  }
  
  # Multiple predictors/features
  if (!is.null(dim(x))) {
    
    # Setup
    n_examples = dim(x)[1]
    n_features = dim(x)[2]
    dj_dw = array(0, c(1,n_features))
    dj_db = 0
    
    # Compute gradient
    for (i in 1:n_examples) {
      f_wb_i = sum(x[i,] * w) + b   # Model prediction with current weights and bias
      error = f_wb_i - y[i]         # Error from the model with n_features on the ith example
      for (j in 1:n_features) {
        dj_dw[j] = dj_dw[j] + error * x[i,j]
      }
      dj_db = dj_db + error
    }
  }
  
  # Compute average over n_examples and return
  dj_dw = dj_dw / n_examples 
  dj_db = dj_db / n_examples
  return(list(dj_dw, dj_db))
    
}