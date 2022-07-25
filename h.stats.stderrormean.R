h.stats.stderrormean <- function(x) {
  return(sd(x)/sqrt(length(na.omit(x))))
}