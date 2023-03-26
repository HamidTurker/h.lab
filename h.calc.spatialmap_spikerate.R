h.calc.spatialmap_spikerate <- function(x_pos, y_pos, x_nbins, y_nbins) {

  # Bin cutoffs
  x.bins <- seq(0, ceiling(max(na.omit(x_pos)))+1, length=x_nbins)
  y.bins <- seq(0, ceiling(max(na.omit(y_pos)))+1, length=y_nbins)
  
  # Map
  #spike_map=as.array(table(findInterval(x_pos, x.bins), findInterval(y_pos, y.bins)))
  
  # findInterval() produces the same bin mapping/table as the following code.
  # table(findInterval()) is faster, but changes the dimensions of the array, which we don't want.
  spike_map <- array(0, c(x_nbins,y_nbins))
  #colnames(spike_map) <- x.bins
  #rownames(spike_map) <- y.bins
  
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
    spike_map[this_xbin,this_ybin]<-spike_map[this_xbin,this_ybin]+1
  }

  return(spike_map)
}

