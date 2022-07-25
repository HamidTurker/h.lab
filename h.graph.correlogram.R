h.graph.correlogram <- function(data = NULL, x.by = NULL, y.by = NULL, order = FALSE, window = NULL) {
  
  
  # Skip all warnings and just 'go for it'?
  if (go == TRUE) {}
  
  
  # Which variables on the x-axis? If none given, assume all.
  
  
  # Which variables on the y-axis? If none given, it's the same ones as the x.by list.
  if (is.null(y.by)) {y.by = x.by}
  
  
  # Over what moving window (in time steps)?
  
  
  # Correlation ordering (Friendly & Kwan, 2002)?
  if (order == T) {}
  
}