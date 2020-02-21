h.effectsize.cohen.d <- function(x, y, type = NULL, missings = NULL) {
  
  # VERSION :: Feb 21 2020
  # Done
  
  # Check arguments
  if (is.null(type)) {print("You must specify the type of Cohen's d you want. >> type = 'between' / 'within' ")}
  else if (is.null(missings)) {print("You must specify whether there's missing data. >> missings = TRUE / FALSE")}
  else if (type=='within' & length(x) != length(y)) {print("Your groups do not have the same number of samples.")}
  
  # Run
  else {

    # No missing data
    if (missings == FALSE) {
    
        # Standardized mean difference for between-subjects designs
        if (type == 'between') {
          adj.x.sd <- sqrt(sum((x-mean(x))^2)/(length(x)-1))
          adj.y.sd <- sqrt(sum((y-mean(y))^2)/(length(y)-1))
          pooled.sd <- sqrt((((length(x)-1)*adj.x.sd^2 + (length(y)-1)*adj.y.sd^2)/(length(x)+length(y)-2)))
          return(abs((mean(x) - mean(y)))/pooled.sd)}
    
        # Standardized mean difference for within-subjects designs
        else if (type == 'within') {
          return(abs(mean(x) - mean(y))/sqrt((sd(x)^2)+(sd(y)^2)-(2*cor(x,y)*sd(x)*sd(y)))*sqrt(2*(1-cor(x,y)))) }}
    
    # Missing data
    else {
      
        # Standardized mean difference for between-subjects designs
        if (type == 'between') {
          adj.x.sd <- sqrt(sum((na.omit(x)-mean(na.omit(x)))^2)/(length(na.omit(x))-1))
          adj.y.sd <- sqrt(sum((na.omit(y)-mean(na.omit(y)))^2)/(length(na.omit(y))-1))
          pooled.sd <- sqrt((((length(na.omit(x))-1)*adj.x.sd^2 + (length(na.omit(y))-1)*adj.y.sd^2)/(length(na.omit(x))+length(na.omit(y))-2)))
          return(abs((mean(na.omit(x)) - mean(na.omit(y))))/pooled.sd)}
      
        # Standardized mean difference for within-subjects designs
        else if (type == 'within') {
          return(abs(mean(na.omit(x)) - mean(na.omit(y)))/sqrt((sd(na.omit(x))^2)+(sd(na.omit(y))^2)-(2*cor(na.omit(x),na.omit(y))*sd(na.omit(x))*sd(na.omit(y))))*sqrt(2*(1-cor(na.omit(x),na.omit(y))))) }}
  }
}