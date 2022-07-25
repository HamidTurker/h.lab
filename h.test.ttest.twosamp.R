h.test.ttest.twosamp <- function(x, y, pairing = NULL, tail = NULL, ci = .95, method = NULL) {
  
  # Feb 21 2020
  # In prep

  #######################################################################################################################
  options(scipen=999)
  options(scipen=0)
  
  pairing='unpaired'
  equal_variances= T
  method='Welch-Satterthwaite'
  var_eq='equal'
  ci=.95
  tail="two"
  
  x = c(1, 2, 32, 23, 4, 23, 4, 5, 55, 6, 2, 5, 24, 9, 10)
  y = c(1, 34, 3, 4, 2, 4, 66, 9, 9, 2, 4, 34, 2, 45)
  z = c(1, 34, 3, 4, 2, 4, 66, 9, 9, 2, 4, 34, 2, 45, 9)
  y = z
  
  x=c(100,200,300,400,500,600,700,800,900,100)
  x=rnorm(100)
  
  x=c(30.02,29.99,30.11,29.97,30.01,29.99)
  y=c(29.89,29.93,29.72,29.98,30.02,29.98)*2
  
  x=c(65,61,63,86,70,55,74,35,72,68,45,58)
  
  #######################################################################################################################
  
  # Check arguments
  if (is.null(pairing)) {message("You must specify the type of pairing for your test. >> type = 'unpaired' / 'paired' ")}
  if (is.null(missings)) {message("You must specify whether there's missing data. >> missings = TRUE / FALSE")}
  if (is.null(tail)) {message("You must specify the tail of the test. >> tail = 'one' / 'two'")}
  if (ci >= 1 | ci <= 0) {message("Please state CI as a value between 0 and 1 (exclusive). >> e.g. ci = .95")}
  
  # Get labels
  label.x <- as.character(substitute(x)); label.y <- as.character(substitute(y))
  
  # Check assumptions
  # Normality
  n_x <- length(x); n_y <- length(y)
  if (is.na(n_x) || n_x < 3L || is.na(n_y) || n_y < 3L) 
    stop("Sample size must be between 3 and 5000, inclusive (following Rahman & Govindarajulu, 2010).")
  h.test.normality(x); h.test.normality(y)
  
  # Run
  n_x = length(x); n_y = length(y)
  mean_x = mean(x); mean_y = mean(y)
  df = n_x+n_y-2
  df_ws = ((var(x)/n_x)+(var(y)/n_y))^2/(((var(x)/n_x)^2/(n_x-1))+((var(y)/n_y)^2/(n_y-1))) # Welch-Satterthwaite degrees of freedom
  pooled_sd = sqrt(((n_x-1)*var(x)+(n_y-1)*var(y))/(n_x+n_y-2))
  meandiff = abs(mean(x)-mean(y)) # Difference between sample means
  SE_meandiff = sqrt((pooled_sd**2/n_x)+(pooled_sd**2/n_y)) # Standard error of the difference between sample means
  
  # Levene's test for equality of variances
  
  # Unpaired / Independent t-test
  if (pairing = 'unpaired') {
    if (n_x == n_y) { # No missing data
        if (equal_variances = T) {        # Equal sample sizes, Equal variances 
          tstat = (mean_x-mean_y)/(pooled_sd*sqrt(2/n_x))
          if (tail == 'one') {pvalue = pt(-abs(tstat), df=df); tailtest="one-tailed"}
          if (tail == 'two') {pvalue = 2*pt(-abs(tstat), df=df); tailtest="two-tailed"}
        } else if (equal_variances = F) {  # Equal sample sizes, Unequal variances
          tstat = (mean_x-mean_y)/(pooled_sd*sqrt(2/n_x)) # Independent/Unpaired t-statistic
          #tstat = (mean_x-mean_y)/(sqrt(((var(x)+var(y))/2))*sqrt(2/n_x))
          #tstat = (mean_x-mean_y)/sqrt((((sum(x^2)-((sum(x)^2)/n_x))+(sum(y^2)-((sum(y)^2)/n_y)))/(n_x+n_y-2))*((1/n_x)+(1/n_y)))
          if (tail == 'one') {pvalue = pt(-abs(tstat), df=df_ws); tailtest="one-tailed"}
          if (tail == 'two') {pvalue = 2*pt(-abs(tstat), df=df_ws); tailtest="two-tailed"}
        
          # Confidence interval
          ci_adj = 1-((1-ci)/2) # Take 1-ci and split that into half to be distributed onto the two tails
          ci_upper = meandiff+qt(ci_adj, df_ws)*SE_meandiff # One half for upper bound (e.g., if ci=.95 (95%), then 2.5% here)
          ci_lower = meandiff-qt(ci_adj, df_ws)*SE_meandiff # Other half for lower bound (remaining 2.5% here, for a total of 5%)
          
          # Print results to console
          cat("H.Lab ::: Student's T-Test:\n
              Method: ",method,", ",pairing,", ",tailtest,"
              Variances are ",var_eq,"\n
              ",label.x,"   n: ",n_x,"   Mean: ",format(round(mean_x,digits=5), nsmall=5)," \tSD: ",sd(x),"
              ",label.y,"   n: ",n_y,"   Mean: ",format(round(mean_y,digits=5), nsmall=5)," \tSD: ",sd(y),"\n
              t(",df_ws,") = ",tstat,", p = ",format(pvalue, scientific=F),"\n
              Difference between sample means: \t\t",meandiff,"
              Standard Error of the mean difference: \t",SE_meandiff,"
              CI at ",ci*100,"% of the mean difference: \t[",ci_lower,"  ",ci_upper,"]", sep='')
        }
    } else {message("Your samples aren't of equal size. Is there missing data?")}
  }
    
  # Paired / Dependent t-test
  if (pairing == 'paired') {
    
    
    
  }
    

    
  
  # Paired / Dependent Student's t-test    
    # No missing data
    if (missings == FALSE) {
      n_x = length(x)
      n_y = length(y)
      mean_x = mean(x)
      mean_y = mean(y)
      x_var = var(x)
      y_var = var(y)
      xy_diff = y-x
      xy_diff_sum = sum(xy_diff)
      xy_diff_sq = xy_diff^2
      xy_diff_sq_sum = sum(xy_diff_sq)
      
      tstat = (xy_diff_sum/n_x)/sqrt( (xy_diff_sq_sum-(xy_diff_sq_sum/n_x)) / ((n_x-1)*n_x) )
      
    
    
    }
}
  