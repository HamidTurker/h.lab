h.dimensionality.pca <- function(data, labels, n_components=NULL, center=TRUE, scale=TRUE) {
  
  n_components=2
  
  # v0.1: 2022 Sep 7
  
  
  # Center the data
  df_centered <- scale(data, center=TRUE, scale=FALSE)
  
  # Covariance matrix
  # (a.k.a. variance, variance-covariance, auto-covariance, or dispersion matrix).
  cov_mat <- cov(df_centered)
  
  # Eigenvectors and eigenvalues
  eigs <- eigen(cov_mat)
  
  # Project data down to dimensions of desired number of dimensions (i.e., n_components)
  # by multiplying the data with the eigenvectors (matrix multiplication)
  projected_mat <- df_centered %*% (eigs$vectors[,1:n_components])
  
}