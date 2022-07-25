h.calc.count <- function(x){
  
  n = length(x)
  x_unique = sort(unique(x))
  frame <- data.frame(
    unique = x_unique,
    count = 0
  )
  for (i in 1:length(x_unique)){
    frame$count[i] = length(subset(x, x == x_unique[i]))
  }
 
  return(frame) 
  
}
