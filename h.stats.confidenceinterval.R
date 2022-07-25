h.stats.confidenceinterval <- function(x, ci) {
  return(sqrt(var(x)/length(x))*qt(1-((1-ci)/2), length(x)-1))
}