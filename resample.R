h.format.resample <- function(timeseries = NULL, groups = NULL, old_samplerate = 1000, new_samplerate = 100) {

  factor = old_samplerate/new_samplerate
  new_timeseries = mean(timeseries[1:new_samplerate])
  for (i in 1:floor(length(timeseries)/factor)-1) {
    new_timeseries = append(new_timeseries, mean(timeseries[(new_samplerate*i)+1:(new_samplerate*i)+new_samplerate]))
  }
  return(new_timeseries)
}

a=(1:1000)
i=2
mean(timeseries[(new_samplerate*i)+1:(new_samplerate*i)+new_samplerate])

new_samplerate=3
mean(new_timeseries[(factor*1):(factor*1)+new_samplerate])

library(signal)
sgolayfilt(a)
new_timeseries = mean(timeseries[1:new_samplerate])

plot(new_timeseries, type='l')
plot(sgolayfilt(a), type='l')
plot(a, type='l')
plot(sgolayfilt(b), type='l')
