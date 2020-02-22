h.test.normality <- function(x, method = "Shapiro-Wilk") {
  
  # VERSION :: Feb 21 2020
  # Sep 13 2019: added Shapiro-Wilk
  
  # Get labels
  label.x <- as.character(substitute(x))
  
  # Run
  if (method == "Shapiro-Wilk") {
    x = x[!is.na(x)]
    n = length(x)
  
    frame <- data.frame(
      index = 1:n,
      data = x,
      sorted = sort(x[complete.cases(x)])
    )
    frame$mi = qnorm((frame$index-.375)/(n+.25))
    frame$ai = 0
    M = sum(frame$mi^2)
    u = 1/sqrt(n)
    frame$ai[n] = (-2.706056*u^5)+(4.434685*u^4)+(-2.071190*u^3)+(-.147981*u^2)+(.221157*u)+(frame$mi[n]*M^(-.5))
    frame$ai[n-1] = (-3.582633*u^5)+(5.682633*u^4)+(-1.752461*u^3)+(-.293762*u^2)+(.042981*u)+(frame$mi[n-1]*M^(-.5))
    frame$ai[1]=-frame$ai[n]
    frame$ai[2]=-frame$ai[n-1]
    epsilon = (M-(2*frame$mi[n]^2)-(2*frame$mi[n-1]^2))/(1-(2*frame$ai[n]^2)-(2*frame$ai[n-1]^2))
    frame$ai[3:(n-2)]=frame$mi[3:(n-2)]/sqrt(epsilon)
    # sum(frame$ai^2) == 1. If not 1, something's wrong.
    W = cor(frame$sorted,frame$ai)^2
    mu = (.0038915*log(n)^3)+(-.083751*log(n)^2)+(-.31082*log(n))-1.5861
    sigma = exp((.0030302*log(n)^2)+(-0.082676*log(n))-0.4803)
    z = (log(1-W)-mu)/sigma
    pvalue = 1-pnorm(z)
    if (pvalue > .05) {message="Data appear to be normally distributed."} else {message="Data appear to NOT be normally distributed."}
  
    # Print results to console
    cat("\nH.Lab - Normality test for ",label.x,":\n
        \t",message,"\n
        \tMethod: ",method,"\n\t\tW=",W,", p=",pvalue,"\n",sep='')}
  
}
