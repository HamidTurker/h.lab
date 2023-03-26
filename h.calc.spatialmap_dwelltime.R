h.calc.spatialmap_dwelltime <- function(x_pos, y_pos, x_nbins, y_nbins, samplerate) {
  
  # Bin cutoffs
  x.bins <- seq(0, ceiling(max(na.omit(x_pos)))+1, length=x_nbins)
  y.bins <- seq(0, ceiling(max(na.omit(y_pos)))+1, length=y_nbins)
  
  # Map
  #dwell_map=table(findInterval(x_pos, x.bins), findInterval(y_pos, y.bins))
  
  # findInterval() produces the same bin mapping/table as the following:
  dwell_map <- array(0, c(x_nbins,y_nbins))
  #colnames(dwell_map) <- x.bins
  #rownames(dwell_map) <- y.bins
  
  for (i in 1:length(na.omit(x_pos))) {
    this_xbin=0
    for (j in 1:length(x.bins)) {
      if (x_pos[i]>x.bins[j]){
        this_xbin=this_xbin+1
      }
    }
    this_ybin=0
    for (j in 1:length(y.bins)) {
      if (y_pos[i]>y.bins[j]){
        this_ybin=this_ybin+1
      }
    }
    dwell_map[this_xbin,this_ybin]<-dwell_map[this_xbin,this_ybin]+1
  }
  
  # Time, in seconds, spent in each bin
  #dwell_map<-dwell_map*samplerate
  
  return(dwell_map)
}
