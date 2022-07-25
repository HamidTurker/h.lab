h.graph.line <- function (formula, dataframe = data, groups = NULL, errors = NULL, ci = NULL, 
                         legend = FALSE, type = "ribbon", text.size = 10, origin = NULL, 
                         y.axis = NULL, y.label = NULL, y.scale = NULL, x.label = NULL, ...) {
  
  dots <- list(...)
  
  ##################################################################
  setwd("C:/Users/Hamid/Desktop/+RESEARCH/+Scripts") # Home
  setwd("/Users/hamid/Desktop/+RESEARCH/+Scripts") # Uris
  setwd("/Users/hamid/Desktop/Research/+Scripts/") # Air
  mydata <- read.csv('dataset.csv') 
  dataframe = mydata
  dataframe$g <- as.factor(c("G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2",
                             "G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G1","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2","G2"))
  formula = clean~time*cue|rec
  groups = quote(g)
  errors = 'ci'; ci = .8
  y.label = NULL; x.label = NULL
  formula = clean~time*cue|rec
  ##################################################################
  
  # Parse the formula
  lhs <- attr(terms(formula), "variables")[2][[1]] # Left hand side of formula (dependent variable)
  rhs <- attr(terms(formula), "variables")[3][[1]] # Right hand side of formula (independent variable)
  
  if ("|" == as.character(rhs)[1]) {rhs_f <- as.name(as.character(rhs)[2]); rhs_p <- as.name(as.character(rhs)[3])} else {
    
  }
  
  
  # Get labels
  if (is.null(y.label)) {label.y <- as.character(lhs)} else {label.y <- y.label}
  if (is.null(x.label)) {label.x <- as.character(rhs)} else {label.x <- x.label}
  if (!is.null(groups)) {label.g <- as.character(groups)}

  
  
  
  
  
  
}