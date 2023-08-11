### h.lab tutorial: machine learning
# Based on the Coursera 'Machine Learning' course
# https://www.coursera.org/learn/machine-learning/home/

# Dependencies
library(ggplot2)
library(plotly)

# h.lab
hlab_dir='~/Desktop/Research/+h.lab/'
source(paste0(hlab_dir,'h.format.expand_grid.R'))
source(paste0(hlab_dir,'h.machinelearning.cost.R'))
source(paste0(hlab_dir,'h.machinelearning.gradient.R'))
source(paste0(hlab_dir,'h.machinelearning.gradient_descent.R'))

##################
##### Week 1 #####
##################
{
  "Gradient descent for linear regression (C1_W1_Lab04)"
  # In this step, we will learn to automate optimization of w and b using gradient descent.
  # We will start with a 'simple' (single variable) linear regression.
  {
    # Our training set contains information on the size of a house (1000 sqft) and its price
    # (in thousands of dollars)
    training_set <- data.frame(
      size = c(1,2),
      price = c(300,500)
    )
    
    # Run gradient descent
    grad_desc = h.machinelearning.gradient_descent(x=training_set$size, y=training_set$price,
                                                   w_init=0, b_init=0, 
                                                   alpha=1.0e-2, num_iters=1e4, 
                                                   verbose=T, search=F, plot=T)
    
  }
  
  
  "VECTORS ::
Vectors, as you will use them in this course, are ordered arrays of numbers. In notation, vectors 
are denoted with lower case bold letters such as 𝐱. The elements of a vector are all the same type.
A vector does not, for example, contain both characters and numbers. The number of elements in the 
array is often referred to as the 'dimension', though mathematicians may prefer 'rank'. The vector shown 
has a dimension of𝑛.

vector = c(77,2,3,10,99,...,n)

The elements of a vector can be referenced with an index. In math settings, 
indexes typically run from 1 to n. In computer science, indexing will often run from 0 to n-1.
In notation, elements of a vector, when referenced individually will indicate the 
index in a subscript, for example, the 0th element, of the vector 𝐱 is 𝑥_0. Note, the x is not bold 
in this case. R is 1-based and so is Matlab, meaning that indexing starts with 1, but Python is 0-based, for example.
This means that in the vector above, to access the first entry in the vector in R, you'd call 'vector[1]'.
In 'Matlab, vector(1)'. But, in Python, it'd be 'vector[0]' to get the number 77 back (since 77 is the corresponding entry)."
}

##################
##### Week 2 #####
##################
{
  "Multiple Variable Linear Regression (C1_W2_Lab02)"
  # Now, we will move to a more complex (multiple variable) linear regression.
  {
    # You will use the motivating example of housing price prediction.
    # The training dataset contains three examples with four features (size, bedrooms, floors, and age of home)
    # shown in the table below. Note that, unlike the previous example, size is in sqft rather than 1000 sqft.
    # This causes an issue, which you will solve in the next section!"
    training_set <- data.frame(
      size = c(2104,1416,852),
      rooms = c(5,3,2),
      floors = c(1,2,1),
      age = c(45,40,35),
      price = c(460,232,178)
    )
    
    # What are the features and what's being predicted?
    features_set=training_set[,c("size","rooms","floors","age")]
    target_set=training_set[,"price"]
    
    # Initialize parameters
    #b_init = 785.1811367994083
    #w_init = c(0.39133535, 18.75376741, -53.36032453, -26.42131618) # One for each feature, in order of columns in the features set
    b_init = 0
    w_init = c(0,0,0,0)
    
    # Run gradient descent
    grad_desc = h.machinelearning.gradient_descent(x=features_set, y=target_set,
                                                   w_init=w_init, b_init=b_init, 
                                                   alpha=5e-7, num_iters=1000, 
                                                   verbose=TRUE, search=FALSE, plot=TRUE, scale=FALSE,
                                                   max_iters=1e+10, conv_crit=1e-50, div_crit=1e+100,
                                                   searchspace=3, cost.method = NULL, gradient.method = NULL)
    
    
    
  }
  
  
  "Polynomial Features

Out of the box, linear regression provides a means of building models of the form:
𝑓(𝐰,𝑏) = 𝑤1𝑥1 + 𝑤2𝑥2 +...+ 𝑤𝑛𝑥𝑛 +𝑏

But what if your features/data are non-linear or are combinations of features? For example, housing prices do not
tend to be linear with living area, but penalize very small or very large houses. How can we use the machinery of 
linear regression to fit this curve? Recall, the 'machinery' we have is the ability to modify the parameters 𝐰 and 𝐛
to fit the model (i.e. the equation) to the training data. However, no amount of adjusting those parameters will achieve
a good fit to a non-linear curve.

Let's try fitting a non-linear curve. We'll start with a simple quadratic:  y = 1 + x^2
"
  
  # Let's engineer some polynomial features and see how our model handles those.
  {
    
    training_set <- data.frame(
      x = 0:19
    )
    training_set$y = (training_set$x^2) + 1 # Thus, the true model of the data is 'y = 1 * x^2 + 1'
    
    # As you can see, the values we're trying to model are quadratic
    plot(training_set$x, training_set$y)
    
    # Descent parameters
    n_iters = 1e4
    alpha = 1.0e-2
    w_initial = 0
    b_initial = 0
    
    # Run gradient descent
    grad_desc = h.machinelearning.gradient_descent(x=training_set$x, y=training_set$y,
                                                   w_init=w_initial, b_init=b_initial, 
                                                   alpha=alpha, num_iters=n_iters, 
                                                   verbose=T, search=F, plot=F)
    # Look at the results! Our best fitting w and b combo is:
    "Gradient descent found w (19) and b (-56), with a cost (J_wb) of 438.9."
    w_fit = grad_desc[[1]]
    b_fit = grad_desc[[2]]
    
    # We can now plot this fitted line over the data itself.
    plot(training_set$x, training_set$y) # original data
    lines(training_set$x, w_fit*training_set$x + b_fit, col="red") # our fitted line, simply plugging our fitted w,b into the model template:  y = w*x + b
    # Okay, that doesn't look to great unfortunately and our model's cost (of ~440) isn't great either.
    
    # Because we know that our y-values are polynomial, we can engineer a feature that has
    # better chances of predicting that. So, let's square our x-values this time and see what happens.
    training_set$x_sq = training_set$x^2
    
    # Run gradient descent on our new squared feature
    alpha = 1e-5
    grad_desc = h.machinelearning.gradient_descent(x=training_set$x_sq, y=training_set$y,
                                                   w_init=w_initial, b_init=b_initial,
                                                   alpha=alpha, num_iters=n_iters, 
                                                   verbose=T, search=F, plot=F)
    # This should have done a much better job.
    w_fit = grad_desc[[1]] # w (Should be ~1.004)
    b_fit = grad_desc[[2]] # b (Should be ~0.049)
    grad_desc[[3]][n_iters,1] # J_wb (Should be ~0.207)
    # In other words, gradient descent is telling us that our model of the data should be following equation:
    # y = 1.004 * x^2 + 0.049   <- This is our computational model
    # Does that make sense? Recall, our model "template" is :   y = w * x + b
    # So, just plug in the values we found with gradient descent. Don't forget that we squared our x-feature, so
    # you need to square it in the final equation of the model too.
    lines(w_fit*training_set$x^2 + b_fit, col="blue")
    # Alright! Our algorithm found the equation 'y = 1.004 * x^2 + .049', which is pretty close to the 'true' model
    # of 'y = 1 * x^2 + 1. Not perfect, but certainly looks better than the first model fit.
    
    
    ### Selecting Features
    # In our examples above, we already knew a priori that a squared term (i.e., x^2) was necessary. But, with
    # real data, that isn't always going to be as obvious (even after visualizing data -- which you should always
    # do regardless). One could try multiple features to find the most useful. For instance, what if
    # we had a first degree term (i.e., x), a second degree term (x^2), and a third degree term (x^3)?
    # Thus, the model we'd be fitting would be: 𝑦=𝑤1𝑥1 + 𝑤2𝑥2^2 + 𝑤3𝑥3^3  + 𝑏?
    
    training_set <- data.frame(
      x1 = 0:19,
      x2 = (0:19)^2,
      x3 = (0:19)^3
    )
    training_set$y = 1 * training_set$x1^2 + 0 # True model
    
    # What are the features and what's being predicted?
    features_set=training_set[,c("x1","x2","x3")]
    target_set=training_set[,"y"]
    
    # Parameters
    n_iters=1e4
    alpha = 1e-7
    b_initial = 0
    w_initial = c(0,0,0) # Recall, we need an initial w for each feature (in this case, 3 of them)
    #w_initial = c(0.1,0.9,0.1)
    
    grad_desc = h.machinelearning.gradient_descent(x=features_set, y=target_set,
                                                   w_init=w_initial, b_init=b_initial,
                                                   alpha=alpha, num_iters=n_iters, 
                                                   verbose=T, search=F, plot=F)
    "Gradient descent found the following values:
      w_1:  0.0823753
      w_2:  0.535521
      w_3:  0.0275222
      b:    0.0105619
      J:    79.0693"
    w1_fit = grad_desc[[1]][1]
    w2_fit = grad_desc[[1]][2]
    w3_fit = grad_desc[[1]][3]
    b_fit = grad_desc[[2]]
    plot(1:20, training_set$y) # our data
    lines(1:20, w1_fit*training_set$x1 + w2_fit*training_set$x2 + w3_fit*training_set$x3 + b_fit, col="blue") # our model
    # Gradient descent is telling us that our final model should be 'y = .08*x + .54*x^2 + .03*x^3 + .0106'
    # In other words, the algorithm is pushing for an emphasis on the second degree term (x^2) and minimizing
    # the impact of the first and third terms. If we ran the algorithm longer, it would further diminish the
    # impact of those two and further emphasize the squared term (by moving the .54 closer and closer to 1.0).
    # The important point, therefore, is that gradient descent can learn to pick the 'correct' features for us.
    # In short, the importance of a feature can be inferred from the weight given to it in the fitted model.
    
    # Since our three features vary in range, let's see what happens when we scale them (by adding scale=TRUE).
    grad_desc = h.machinelearning.gradient_descent(x=features_set, y=target_set,
                                                   w_init=w_initial, b_init=b_initial,
                                                   alpha=alpha, num_iters=n_iters, 
                                                   verbose=T, search=F, plot=F,
                                                   scale=T)
    "Gradient descent found the following values:
      w_1:  0.106637
      w_2:  0.110469
      w_3:  0.108901
      b:    0.123438
      J:    14016"
    w1_fit = grad_desc[[1]][1]; w2_fit = grad_desc[[1]][2]; w3_fit = grad_desc[[1]][3]; b_fit = grad_desc[[2]]
    lines(1:20, w1_fit*scale(training_set$x1) + w2_fit*scale(training_set$x2) + w3_fit*scale(training_set$x3) + b_fit, col="red")
    # What happened here?
    
    # What if we change the learning rate to be a bigger number?
    alpha=1e-1; n_iters=1e5
    #alpha=.6; n_iters=1e3
    grad_desc = h.machinelearning.gradient_descent(x=features_set, y=target_set,
                                                   w_init=w_initial, b_init=b_initial,
                                                   alpha=alpha, num_iters=n_iters, 
                                                   verbose=T, search=F, plot=F,
                                                   scale=T)
    "Gradient descent found the following values:
      w_1:  0.000104596
      w_2:  116.443
      w_3:  0.000167483
      b:    123.5
      J:    6.90045e-11"
    w1_fit = grad_desc[[1]][1]; w2_fit = grad_desc[[1]][2]; w3_fit = grad_desc[[1]][3]; b_fit = grad_desc[[2]]
    lines(w1_fit*scale(training_set$x1) + w2_fit*scale(training_set$x2) + w3_fit*scale(training_set$x3)  + b_fit, col="green")
    # Okay, our cost has dropped enormously and both w_1 and w_3 have effectively been eliminated in their importance.
    # This shows us that we really need to pay attention to all the elements of our data and the models we run on them:
    # the learning rate, the final cost, the number of iterations, whether our data are scaled or not, and so on.
    
    # Depending on whether you're running these algorithms in R, python, or Matlab, or Windows versus Mac,
    # you might have noticed that it can take a long time to run. Try playing more with the alpha and n_iters
    # while paying attention to how quickly the cost (J) drops. The algorithm will, of course, finish faster if
    # you ask for fewer n_iters. But you can also try a higher learning rate, so see if the algorithm can bring
    # the cost down at a faster pace (e.g., from 1e-1, you can try .3, .6. or .9). At some point, you will
    # end up with a learning rate that's too high and the algorithm will diverge (instead of converge), meaning
    # that the cost kept going up at each iteration instead of going down. Scaling can also speed up processing.
    # Playing around with these (and other) parameters for a while can already give you strong suggestions as to
    # which features are helpful and which aren't (at least, relative to each other). That way, you may be able to
    # speed up your work by quickly noticing which features can be dropped and then reducing the work that gradient
    # descent has to do with a smaller set of features.
    
    
    
    ### Complex function
    # With feature engineering, we can even model quite complex functions.
    xvals = 0:19
    training_set <- data.frame(
      x1 = xvals,
      x2 = xvals^2,
      x3 = xvals^3,
      x4 = xvals^4,
      x5 = xvals^5,
      x6 = xvals^6,
      x7 = xvals^7,
      x8 = xvals^8,
      x9 = xvals^9,
      x10 = xvals^10,
      x11 = xvals^11,
      x12 = xvals^12,
      x13 = xvals^13
    )
    training_set$y = cos(xvals/2) # True model
    
    # What are the features and what's being predicted?
    features_set=training_set[,1:13] # The first 13 columns, which are our 13 features
    target_set=training_set[,"y"]
    plot(xvals, training_set$y) # our data
    
    # Parameters
    n_iters=1e5
    alpha = .1
    b_initial = 0
    w_initial = c(0,0,0,0,0,0,0,0,0,0,0,0,0)
    
    # This will take a long time in R. Very long!
    grad_desc = h.machinelearning.gradient_descent(x=features_set, y=target_set,
                                                   w_init=w_initial, b_init=b_initial,
                                                   alpha=alpha, num_iters=n_iters, 
                                                   verbose=T, search=F, plot=F,
                                                   scale=T)
    "Gradient descent found the following values:
      w_1:  -3.81903
      w_2:  3.76706
      w_3:  6.43265
      w_4:  0.298999
      w_5:  -4.22198
      w_6:  -4.99168
      w_7:  -3.21612
      w_8:  -0.570534
      w_9:  1.69477
      w_10:  2.85959
      w_11:  2.61382
      w_12:  0.91283
      w_13:  -2.13653
      b:    -0.00728719
      J:    0.0172613"
    lines(xvals, -3.81903*scale(training_set$x1) + 3.76706*scale(training_set$x2) + 6.43265*scale(training_set$x3) +
                 0.298999*scale(training_set$x4) + -4.22198*scale(training_set$x5) + -4.99168*scale(training_set$x6) + 
                 -3.21612*scale(training_set$x7) + -0.570534*scale(training_set$x8) + 1.69477*scale(training_set$x9) + 
                 2.85959*scale(training_set$x10) + 2.61382*scale(training_set$x11) + 0.91283*scale(training_set$x12) +
                -2.13653*scale(training_set$x13) + -0.00728719, col="blue")
    # Not bad!
    
    # There are ways to speed up the process in R, but that matter lies beyond the scope of these sessions.
    # R's focus is on running algebraic computations in a stable and accurate manner, rather than with a
    # trade-off in accuracy to speed things up. For many purposes, this trade-off in accuracy is probably
    # negligible, so feel free to look into R Open, GotoBlas, OpenBLAS, MKL, and related software that can
    # speed up algebraic processes in R. Do note that in, e.g., Python or Matlab, these processes are already
    # much faster. So, you may consider running some gradient descent algorithms in Python or Matlab instead.
    
  }
  
}

##################
##### Week 3 #####
##################
{
  "Classification"
  # In addition to predicting values using a model, another common machine learning problem is that of
  # classifying data points into categories. For classification problems, our linear regression approach
  # is - sadly - not very suited. Instead, we'll need something called logistic regression.
  
  # Let's see how they both perform by setting up some data (x, y) that has groupings (g) and a color map (c)
  training_set1a <- data.frame(
    x = c(0, 1, 2, 3, 4, 5),
    g = c(0, 0, 0, 1, 1, 1),
    c = c("blue","blue","blue","red","red","red")
  )
  plot(training_set1a$x, training_set1a$g, col=training_set1a$c, pch=16) # One variable (x)
  
  # Run gradient descent
  grad_desc = h.machinelearning.gradient_descent(x=training_set1a$x, y=training_set1a$g,
                                                 w_init=0, b_init=0, 
                                                 alpha=.1, num_iters=1e6, 
                                                 verbose=T, search=T, plot=T)
  "Gradient descent found w (0.257143) and b (-0.142857), with a cost (J_wb) of 0.0285714."
  lines(training_set1a$x, 0.257143*training_set1a$x + -0.142857, lwd=4) # Fitted linear regression line
  # Note, lines() takes an x and y argument, so we first pass it the independent x values and then the model
  
  # We can then draw a threshold, say at g = .5
  abline(h=.5) # abline will now draw a horizontal line at .5 (hence, h=.5)
  # Next, for example, we decide to classify points that lie to one side of the intersection
  # between our threshold and our regression line will be classified as one group and points
  # on the other side as part of the other group.
  f1 <- function(x) 0.257143*x + -0.142857 # Our fitted model
  f2 <- function() .5                      # Our arbitrary threshold
  
  uniroot(function(x) f1(x)-f2(),c(0,5))$root # This asks for the intersection (root) between
  # two functions in the interval 0 to 5. And indeed, it finds x is 2.499998 -- that looks right.
  abline(v=uniroot(function(x) f1(x)-f2(),c(0,5))$root) # abline will now draw a vertical line at ~2.5
  # In this case, all data are classified correctly (blue on the left side of ~2.5, red on the right side).
  
  # But now let's add another data point
  training_set1b <- data.frame(
    x = c(0, 1, 2, 3, 4, 5, 10),
    g = c(0, 0, 0, 1, 1, 1, 1),
    c = c("blue","blue","blue","red","red","red","red")
  )
  plot(training_set1b$x, training_set1b$g, col=training_set1b$c, pch=16) # One variable (x)
  
  # Run gradient descent
  grad_desc = h.machinelearning.gradient_descent(x=training_set1b$x, y=training_set1b$g,
                                                 w_init=0, b_init=0, 
                                                 alpha=.01, num_iters=1e6, 
                                                 verbose=T, search=F, plot=F)
  "Gradient descent found w (0.117391) and b (0.152174), with a cost (J_wb) of 0.057764."
  
  # Plot the model
  lines(training_set1b$x, 0.117391*training_set1b$x + -0.152174, lwd=4) # Fitted linear regression line
  abline(h=.5) # Threshold
  f1 <- function(x) 0.117391*x + -0.152174 # Our fitted model
  f2 <- function() .5                      # Our arbitrary threshold
  abline(v=uniroot(function(x) f1(x)-f2(),c(0,10))$root) # abline will now draw a vertical line at ~5.56
  
  # Okay, that looks bad! We're now misclassifying the original 3 red data points and only capturing the
  # new data point that we just introduced. Clearly, we need a better approach than linear regression.
  
  "Logistic regression"
  # Enter logistic regression, one of the (if actually the) most used classification algorithm in the world!
  # Instead of a linear model 'y(x) = w*x + b', we will use a so-called sigmoid function (a.k.a. logistic
  # function). The sigmoid function is 'g(z) = 1 / (1+e^-z)', where 0 < g(z) < 1 (so, y can only take on values
  # between 0 and 1). Importantly, the z variable in the sigmoid function is actually y(x), the output of the
  # linear regression function. Let's take a closer look, using training_set2.
  
  
  
  

  
  
}







training_set2 <- data.frame(
  x1 = c(.5, 1, 1, 1.5, 2, 3),
  x2 = c(1.5, 1, 2.5, .5, 2, .5),
  g = c(0, 0, 1, 0, 1, 1),
  c = c("red","red","blue","red","blue","blue")
)
plot(training_set2$x1, training_set2$x2, col=training_set2$c, pch=16)  # Two variables (x1, x2)

features_set=training_set2[,c("x1","x2")]
target_set=training_set2[,"g"]

x=features_set; y=target_set;
w_init=c(1,1); b_init=-3; 
alpha=.1; num_iters=1e6; 
verbose=T; search=F; plot=F

desc_hist = array(NA, c(num_iters,3)); colnames(desc_hist) = c("J","w","b")
n_examples = length(x)
b = b_init
w = w_init
i = 1







# What about with 2 variables? Things get even more complicated.
training_set2 <- data.frame(
  x1 = c(.5, 1, 1, 1.5, 2, 3),
  x2 = c(1.5, 1, 2.5, .5, 2, .5),
  g = c(0, 0, 1, 0, 1, 1),
  c = c("red","red","blue","red","blue","blue")
)
plot(training_set2$x1, training_set2$x2, col=training_set2$c, pch=16)  # Two variables (x1, x2)

# Run gradient descent
grad_desc = h.machinelearning.gradient_descent(x=training_set2$x1, y=training_set2$x2,
                                               w_init=0, b_init=0, 
                                               alpha=.1, num_iters=1e6, 
                                               verbose=T, search=F, plot=F)
"Gradient descent found w (-0.375) and b (1.89583), with a cost (J_wb) of 0.230903."
plot(training_set2$x1, training_set2$x2, col=training_set2$c, pch=16) # Two variables (x1, x2)
lines(training_set2$x1, -0.375*training_set2$x1 + 1.89583, lwd=4) # Fitted linear regression line













###################
##### FIGURES #####
###################

# Figure 2
{
  # Features
  training_set <- data.frame(
    x1 = 0:19,
    x2 = (0:19)^2,
    x3 = (0:19)^3
  )
  
  # Plot of data
  training_set$y = 1 * training_set$x1^2 + 0 # True model
  plot(1:20, training_set$y)
  
  # Unscaled
  w1_fit= 0.0823753;
  w2_fit= 0.535521;
  w3_fit= 0.0275222;
  b_fit= 0.0105619;
  lines(w1_fit*training_set$x1 + w2_fit*training_set$x2 + w3_fit*training_set$x3 + b_fit, col="blue")
  
  # Scaled, but same learning rate
  w1_fit= 0.106637;
  w2_fit= 0.110469;
  w3_fit= 0.108901;
  b_fit= 0.123438;
  lines(w1_fit*scale(training_set$x1) + w2_fit*scale(training_set$x2) + w3_fit*scale(training_set$x3) + b_fit, col="red")
  
  # Scaled and adjusted learning rate
  w1_fit= 0.000104596;
  w2_fit= 116.443;
  w3_fit= 0.000167483;
  b_fit= 123.5;
  lines(w1_fit*scale(training_set$x1) + w2_fit*scale(training_set$x2) + w3_fit*scale(training_set$x3)  + b_fit, col="green")
}


















if (TRUE) {
x=list(c(1.24e+03, 3.00e+00, 1.00e+00, 6.40e+01),
      c(1.95e+03, 3.00e+00, 2.00e+00, 1.70e+01),
      c(1.72e+03, 3.00e+00, 2.00e+00, 4.20e+01),
      c(1.96e+03, 3.00e+00, 2.00e+00, 1.50e+01),
      c(1.31e+03, 2.00e+00, 1.00e+00, 1.40e+01),
      c(8.64e+02, 2.00e+00, 1.00e+00, 6.60e+01),
      c(1.84e+03, 3.00e+00, 1.00e+00, 1.70e+01),
      c(1.03e+03, 3.00e+00, 1.00e+00, 4.30e+01),
      c(3.19e+03, 4.00e+00, 2.00e+00, 8.70e+01),
      c(7.88e+02, 2.00e+00, 1.00e+00, 8.00e+01),
      c(1.20e+03, 2.00e+00, 2.00e+00, 1.70e+01),
      c(1.56e+03, 2.00e+00, 1.00e+00, 1.80e+01),
      c(1.43e+03, 3.00e+00, 1.00e+00, 2.00e+01),
      c(1.22e+03, 2.00e+00, 1.00e+00, 1.50e+01),
      c(1.09e+03, 2.00e+00, 1.00e+00, 6.40e+01),
      c(8.48e+02, 1.00e+00, 1.00e+00, 1.70e+01),
      c(1.68e+03, 3.00e+00, 2.00e+00, 2.30e+01),
      c(1.77e+03, 3.00e+00, 2.00e+00, 1.80e+01),
      c(1.04e+03, 3.00e+00, 1.00e+00, 4.40e+01),
      c(1.65e+03, 2.00e+00, 1.00e+00, 2.10e+01),
      c(1.09e+03, 2.00e+00, 1.00e+00, 3.50e+01),
      c(1.32e+03, 3.00e+00, 1.00e+00, 1.40e+01),
      c(1.59e+03, 0.00e+00, 1.00e+00, 2.00e+01),
      c(9.72e+02, 2.00e+00, 1.00e+00, 7.30e+01),
      c(1.10e+03, 3.00e+00, 1.00e+00, 3.70e+01),
      c(1.00e+03, 2.00e+00, 1.00e+00, 5.10e+01),
      c(9.04e+02, 3.00e+00, 1.00e+00, 5.50e+01),
      c(1.69e+03, 3.00e+00, 1.00e+00, 1.30e+01),
      c(1.07e+03, 2.00e+00, 1.00e+00, 1.00e+02),
      c(1.42e+03, 3.00e+00, 2.00e+00, 1.90e+01),
      c(1.16e+03, 3.00e+00, 1.00e+00, 5.20e+01),
      c(1.94e+03, 3.00e+00, 2.00e+00, 1.20e+01),
      c(1.22e+03, 2.00e+00, 2.00e+00, 7.40e+01),
      c(2.48e+03, 4.00e+00, 2.00e+00, 1.60e+01),
      c(1.20e+03, 2.00e+00, 1.00e+00, 1.80e+01),
      c(1.84e+03, 3.00e+00, 2.00e+00, 2.00e+01),
      c(1.85e+03, 3.00e+00, 2.00e+00, 5.70e+01),
      c(1.66e+03, 3.00e+00, 2.00e+00, 1.90e+01),
      c(1.10e+03, 2.00e+00, 2.00e+00, 9.70e+01),
      c(1.78e+03, 3.00e+00, 2.00e+00, 2.80e+01),
      c(2.03e+03, 4.00e+00, 2.00e+00, 4.50e+01),
      c(1.78e+03, 4.00e+00, 2.00e+00, 1.07e+02),
      c(1.07e+03, 2.00e+00, 1.00e+00, 1.00e+02),
      c(1.55e+03, 3.00e+00, 1.00e+00, 1.60e+01),
      c(1.95e+03, 3.00e+00, 2.00e+00, 1.60e+01),
      c(1.22e+03, 2.00e+00, 2.00e+00, 1.20e+01),
      c(1.62e+03, 3.00e+00, 1.00e+00, 1.60e+01),
      c(8.16e+02, 2.00e+00, 1.00e+00, 5.80e+01),
      c(1.35e+03, 3.00e+00, 1.00e+00, 2.10e+01),
      c(1.57e+03, 3.00e+00, 1.00e+00, 1.40e+01),
      c(1.49e+03, 3.00e+00, 1.00e+00, 5.70e+01),
      c(1.51e+03, 2.00e+00, 1.00e+00, 1.60e+01),
      c(1.10e+03, 3.00e+00, 1.00e+00, 2.70e+01),
      c(1.76e+03, 3.00e+00, 2.00e+00, 2.40e+01),
      c(1.21e+03, 2.00e+00, 1.00e+00, 1.40e+01),
      c(1.47e+03, 3.00e+00, 2.00e+00, 2.40e+01),
      c(1.77e+03, 3.00e+00, 2.00e+00, 8.40e+01),
      c(1.65e+03, 3.00e+00, 1.00e+00, 1.90e+01),
      c(1.03e+03, 3.00e+00, 1.00e+00, 6.00e+01),
      c(1.12e+03, 2.00e+00, 2.00e+00, 1.60e+01),
      c(1.15e+03, 3.00e+00, 1.00e+00, 6.20e+01),
      c(8.16e+02, 2.00e+00, 1.00e+00, 3.90e+01),
      c(1.04e+03, 3.00e+00, 1.00e+00, 2.50e+01),
      c(1.39e+03, 3.00e+00, 1.00e+00, 6.40e+01),
      c(1.60e+03, 3.00e+00, 2.00e+00, 2.90e+01),
      c(1.22e+03, 3.00e+00, 1.00e+00, 6.30e+01),
      c(1.07e+03, 2.00e+00, 1.00e+00, 1.00e+02),
      c(2.60e+03, 4.00e+00, 2.00e+00, 2.20e+01),
      c(1.43e+03, 3.00e+00, 1.00e+00, 5.90e+01),
      c(2.09e+03, 3.00e+00, 2.00e+00, 2.60e+01),
      c(1.79e+03, 4.00e+00, 2.00e+00, 4.90e+01),
      c(1.48e+03, 3.00e+00, 2.00e+00, 1.60e+01),
      c(1.04e+03, 3.00e+00, 1.00e+00, 2.50e+01),
      c(1.43e+03, 3.00e+00, 1.00e+00, 2.20e+01),
      c(1.16e+03, 3.00e+00, 1.00e+00, 5.30e+01),
      c(1.55e+03, 3.00e+00, 2.00e+00, 1.20e+01),
      c(1.98e+03, 3.00e+00, 2.00e+00, 2.20e+01),
      c(1.06e+03, 3.00e+00, 1.00e+00, 5.30e+01),
      c(1.18e+03, 2.00e+00, 1.00e+00, 9.90e+01),
      c(1.36e+03, 2.00e+00, 1.00e+00, 1.70e+01),
      c(9.60e+02, 3.00e+00, 1.00e+00, 5.10e+01),
      c(1.46e+03, 3.00e+00, 2.00e+00, 1.60e+01),
      c(1.45e+03, 3.00e+00, 2.00e+00, 2.50e+01),
      c(1.21e+03, 2.00e+00, 1.00e+00, 1.50e+01),
      c(1.55e+03, 3.00e+00, 2.00e+00, 1.60e+01),
      c(8.82e+02, 3.00e+00, 1.00e+00, 4.90e+01),
      c(2.03e+03, 4.00e+00, 2.00e+00, 4.50e+01),
      c(1.04e+03, 3.00e+00, 1.00e+00, 6.20e+01),
      c(1.62e+03, 3.00e+00, 1.00e+00, 1.60e+01),
      c(8.03e+02, 2.00e+00, 1.00e+00, 8.00e+01),
      c(1.43e+03, 3.00e+00, 2.00e+00, 2.10e+01),
      c(1.66e+03, 3.00e+00, 1.00e+00, 6.10e+01),
      c(1.54e+03, 3.00e+00, 1.00e+00, 1.60e+01),
      c(9.48e+02, 3.00e+00, 1.00e+00, 5.30e+01),
      c(1.22e+03, 2.00e+00, 2.00e+00, 1.20e+01),
      c(1.43e+03, 2.00e+00, 1.00e+00, 4.30e+01),
      c(1.66e+03, 3.00e+00, 2.00e+00, 1.90e+01),
      c(1.21e+03, 3.00e+00, 1.00e+00, 2.00e+01),
      c(1.05e+03, 2.00e+00, 1.00e+00, 6.50e+01))
y=c(300.  , 509.8 , 394.  , 540.  , 415.  , 230.  , 560.  , 294.  ,
    718.2 , 200.  , 302.  , 468.  , 374.2 , 388.  , 282.  , 311.8 ,
    401.  , 449.8 , 301.  , 502.  , 340.  , 400.28, 572.  , 264.  ,
    304.  , 298.  , 219.8 , 490.7 , 216.96, 368.2 , 280.  , 526.87,
    237.  , 562.43, 369.8 , 460.  , 374.  , 390.  , 158.  , 426.  ,
    390.  , 277.77, 216.96, 425.8 , 504.  , 329.  , 464.  , 220.  ,
    358.  , 478.  , 334.  , 426.98, 290.  , 463.  , 390.8 , 354.  ,
    350.  , 460.  , 237.  , 288.3 , 282.  , 249.  , 304.  , 332.  ,
    351.8 , 310.  , 216.96, 666.34, 330.  , 480.  , 330.3 , 348.  ,
    304.  , 384.  , 316.  , 430.4 , 450.  , 284.  , 275.  , 414.  ,
    258.  , 378.  , 350.  , 412.  , 373.  , 225.  , 390.  , 267.4 ,
    464.  , 174.  , 340.  , 430.  , 440.  , 216.  , 329.  , 388.  ,
    390.  , 356.  , 257.8 )
} # Load large housing price data set

size=NULL; rooms=NULL; floors=NULL; age=NULL
for (i in 1:length(x)) {
  size = append(size, x[[i]][1])
  rooms = append(rooms, x[[i]][2])
  floors = append(floors, x[[i]][3])
  age = append(age, x[[i]][4])
}

training_set <- data.frame(
  size = size,
  rooms = rooms,
  floors = floors,
  age = age,
  price = y
)

plot(training_set$size,training_set$price)
plot(training_set$rooms,training_set$price)
plot(training_set$floors,training_set$price)
plot(training_set$age,training_set$price)

features_set=training_set[,c("size","rooms","floors","age")]
target_set=training_set[,"price"]
x=features_set
y=target_set

w_init=c(0,0,0,0)
b_init=0
tmp_alpha=1e-7
iters=10

grad_desc = h.machinelearning.gradient_descent(x=features_set, y=target_set,
                                               w_in=w_init, b_in=b_init, 
                                               alpha=tmp_alpha, num_iters=iters, 
                                               verbose=TRUE, search=FALSE, plot=TRUE, scale=FALSE)


training_set <- data.frame(
  size = scale(size),
  rooms = scale(rooms),
  floors = scale(floors),
  age = scale(age),
  price = scale(y)
)

plot(training_set$size,training_set$age)


features_set=training_set[,c("size","rooms","floors","age")]
target_set=training_set[,"price"]
x=features_set
y=target_set
tmp_alpha=1.0e-1 
grad_desc = h.machinelearning.gradient_descent(x=features_set, y=target_set,
                                               w_in=w_init, b_in=b_init, 
                                               alpha=tmp_alpha, num_iters=1000, 
                                               verbose=TRUE, search=FALSE, plot=TRUE, scale=TRUE)





x = c(6.1101,  5.5277,  8.5186,  7.0032,  5.8598,  8.3829,  7.4764,
   8.5781,  6.4862,  5.0546,  5.7107, 14.164 ,  5.734 ,  8.4084,
   5.6407,  5.3794,  6.3654,  5.1301,  6.4296,  7.0708,  6.1891,
   20.27  ,  5.4901,  6.3261,  5.5649, 18.945 , 12.828 , 10.957 ,
   13.176 , 22.203 ,  5.2524,  6.5894,  9.2482,  5.8918,  8.2111,
   7.9334,  8.0959,  5.6063, 12.836 ,  6.3534,  5.4069,  6.8825,
   11.708 ,  5.7737,  7.8247,  7.0931,  5.0702,  5.8014, 11.7   ,
   5.5416,  7.5402,  5.3077,  7.4239,  7.6031,  6.3328,  6.3589,
   6.2742,  5.6397,  9.3102,  9.4536,  8.8254,  5.1793, 21.279 ,
   14.908 , 18.959 ,  7.2182,  8.2951, 10.236 ,  5.4994, 20.341 ,
   10.136 ,  7.3345,  6.0062,  7.2259,  5.0269,  6.5479,  7.5386,
   5.0365, 10.274 ,  5.1077,  5.7292,  5.1884,  6.3557,  9.7687,
   6.5159,  8.5172,  9.1802,  6.002 ,  5.5204,  5.0594,  5.7077,
   7.6366,  5.8707,  5.3054,  8.2934, 13.394 ,  5.4369)
y = c(17.592  ,  9.1302 , 13.662  , 11.854  ,  6.8233 , 11.886  ,
      4.3483 , 12.     ,  6.5987 ,  3.8166 ,  3.2522 , 15.505  ,
      3.1551 ,  7.2258 ,  0.71618,  3.5129 ,  5.3048 ,  0.56077,
      3.6518 ,  5.3893 ,  3.1386 , 21.767  ,  4.263  ,  5.1875 ,
      3.0825 , 22.638  , 13.501  ,  7.0467 , 14.692  , 24.147  ,
      -1.22   ,  5.9966 , 12.134  ,  1.8495 ,  6.5426 ,  4.5623 ,
      4.1164 ,  3.3928 , 10.117  ,  5.4974 ,  0.55657,  3.9115 ,
      5.3854 ,  2.4406 ,  6.7318 ,  1.0463 ,  5.1337 ,  1.844  ,
      8.0043 ,  1.0179 ,  6.7504 ,  1.8396 ,  4.2885 ,  4.9981 ,
      1.4233 , -1.4211 ,  2.4756 ,  4.6042 ,  3.9624 ,  5.4141 ,
      5.1694 , -0.74279, 17.929  , 12.054  , 17.054  ,  4.8852 ,
      5.7442 ,  7.7754 ,  1.0173 , 20.992  ,  6.6799 ,  4.0259 ,
      1.2784 ,  3.3411 , -2.6807 ,  0.29678,  3.8845 ,  5.7014 ,
      6.7526 ,  2.0576 ,  0.47953,  0.20421,  0.67861,  7.5435 ,
      5.3436 ,  4.2415 ,  6.7981 ,  0.92695,  0.152  ,  2.8214 ,
      1.8451 ,  4.2959 ,  7.2029 ,  1.9869 ,  0.14454,  9.0551 ,
      0.61705)
plot(x,y)

grad_desc = h.machinelearning.gradient_descent(x=x, y=y,
                                               w_in=100, b_in=100, 
                                               alpha=.001, num_iters=5000, 
                                               verbose=TRUE, search=TRUE, plot=TRUE, scale=FALSE)



