h.test.goodnessoffit.ks <- function(x, y, method = "Kolmogorov-Smirnov") {
  
  x=c(rep(100,8),rep(200,25),rep(300,88),rep(400,172),rep(500,243),rep(600,252),rep(700,144),rep(800,49),rep(900,13),rep(1000,6))
  write.csv(x, file='out.csv',row.names = F)
  
  x = x[!is.na(x)]
  x_unique = sort(unique(x[!is.na(x)]))
  n=length(x)
  frame=NULL
  
  # Theoretical CDF
  tCDF = ecdf(rnorm(1000000, mean=mean(x), sd=sd(x)))
  
  frame <- data.frame(
    index = 1:length(x_unique),
    sorted = x_unique,
    freq = h.calc.frequencytable(x)[,2],
    cum_freq = h.calc.frequencytable(x)[,3],
    Sn = h.calc.frequencytable(x)[,3]/n,
    zscore_x = scale(x_unique)
  )
  frame$Fn = tCDF(frame$sorted)
  frame$Dn = frame$Sn-frame$Fn
  Dn_max = max(abs(frame$Dn))
}

Fn(1000)
