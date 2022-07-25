h.calc.frequencytable <- function(x){
  
  n = length(x)
  x_unique = sort(unique(x))
  frame = NULL
  frame <- data.frame(
    sorted_unique = x_unique
  )
  for (i in 1:length(x_unique)){frame$frequency[i] = length(subset(x, x == x_unique[i]))}
  frame$cumulative_frequency = frame$frequency[1]
  for (i in 2:length(x_unique)){frame$cumulative_frequency[i] = frame$frequency[i]+frame$cumulative_frequency[i-1]}
  
  return(frame) 
  
}
