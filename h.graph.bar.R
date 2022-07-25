h.graph.bar <- function (model, dataframe = data, errors = NULL, ci = NULL, 
                         legend = FALSE, fill = "solid", text.size = 10, origin = NULL, 
                         y.axis = NULL, y.label = NULL, y.scale = NULL, x.label = NULL, ...) {
  
  
  # VERSION ::: Feb 25 2020
  
  dots <- list(...)
  
  ##################################################################
  setwd("C:/Users/Hamid/Desktop/RESEARCH/+Scripts") # Home
  setwd("/Users/hamid/Desktop/RESEARCH/+Scripts") # Uris
  setwd("/Users/hamid/Desktop/Research/+Scripts/") # Air
  mydata <- read.csv('dataset.csv') 
  df = mydata
  df$g <- as.factor(c("G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2",
                             "G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2"))
  model = clean~time|rec
  form = model
  model = clean~time*age+hemi|rec
  groups = quote(g)
  errors = 'ci'; ci = .8
  ##################################################################
  
  # Parse the model and bind the formula to the data
  formula <- as.formula(model)
  model.parse <- h.tools.model.parse(model)
  model.matrix <- h.tools.model.matrix(model.parse)
  model.data <- h.tools.model.data(model.matrix, data)
  model.fit <- h.tools.model.fit(model.data)
  
  # Get labels
  label.model <- as.character(deparse(model))
  label.x <- as.character(deparse(lhs))
  label.y <- as.character(substitute(y))

  # Subset of dataframe that is we'll be using
  if (is.null(groups)) {active.data <- data.frame(x = as.factor(eval(rhs, dataframe)), y = eval(lhs, dataframe))
  } else {active.data <- data.frame(groups = as.factor(eval(groups, dataframe)), x = as.factor(eval(rhs, dataframe)), y = eval(lhs, dataframe))}
  
  # Get labels
  if (is.null(y.label)) {label.y <- deparse(lhs)} else {label.y <- y.label}
  if (is.null(x.label)) {label.x <- deparse(rhs)} else {label.x <- x.label}
  if (!is.null(groups)) {label.g <- deparse(groups)}
  
  # Groups & Levels
  if (is.null(groups)) {n.groups <- 1L} else {n.groups <- length(levels(active.data$groups))}
  n.levels <- length(levels(active.data$x))
  
  # Means & Errors
  means <- matrix(nrow = n.groups, ncol = n.levels)
  errorbars <- matrix(nrow = n.groups, ncol = n.levels)
  for (i in 1:n.groups) {     # For the number of groups..
    for (j in 1:n.levels) {   # For the number of levels..
      if (n.groups == 1) {    # If you we only have one group..
        means[i,j] <- with(active.data, tapply(y, x, mean))[j]                                                              # Mean
        if (errors == 'sem') {errorbars[i,j] <- with(active.data, tapply(y, x, function(y) {h.stats.stderrormean(y)}))[j]}  # Error bar is standard error of the mean
        else if (errors == 'sd') errorbars[i,j] <- with(active.data, tapply(y, x, function(y) {sd(y)}))[j]                  # Error bar is standard deviation
        else if (errors == 'ci') {                                                                                          # CI is adjusted based on degrees of freedom of the sample
          if (is.null(ci)) {print("To compute a confidence interval for the error bars, specify a percentage. >> e.g.: ci = .95")}
          else errorbars[i,j] <- with(active.data, tapply(y, x, function(y) {h.stats.confidenceinterval(y, ci)}))[j]}
      } else {                # If we have more than one group..
        means[i,j] <- with(active.data, tapply(y, list(groups, x), mean))[i,j]                                                              # Mean
        if (errors == 'sem') {errorbars[i,j] <- with(active.data, tapply(y, list(groups, x), function(y) {h.stats.stderrormean(y)}))[i,j]}  # Error bar is standard error of the mean
        else if (errors == 'sd') errorbars[i,j] <- with(active.data, tapply(y, list(groups, x), function(y) {sd(y)}))[i,j]                  # Error bar is standard deviation
        else if (errors == 'ci') {                                                                                                          # CI is adjusted based on degrees of freedom of the sample
          if (is.null(ci)) {print("To compute a confidence interval for the error bars, specify a percentage. >> e.g.: ci = .95")}
          else errorbars[i,j] <- with(active.data, tapply(y, list(groups, x), function(y) {h.stats.confidenceinterval(y, ci)}))[i,j]}
      }
    }
  }
 
  
  
  
  
  
   
} # End

