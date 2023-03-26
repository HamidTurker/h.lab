h.calc.heading <- function(x_pos, y_pos) {
  
  # v0.1: 2022 July 26
  
  # Calculate the heading (head direction) through the four-quadrant inverse tangent
  
  heading <- atan2(y_pos,x_pos)+pi/2
  heading[heading < 0] <- heading[heading < 0]+2*pi
  
  return(heading)
}
