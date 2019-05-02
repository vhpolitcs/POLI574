#Problem set 1

## 1. Suppose that 20 people {Ann, Bill, Charlie, Daria, . . . } are seated randomly around a round table. What is the probability that Ann and Bill are seated next to one another?
  
# I am created a function called "table_func" which is based on a given number of trials (see: "trials").
# I create a vector called "people_vec" which is made by the in-built command LETTERS and set it to be form 1 to 20

a <- LETTERS
a

# Then, I create a container called "sum" that is going to capture the results of my trial runs.
# Now, I am indicating that I want this for 'i' in 1:trials, which means that in the first trial, i is 1, in the second trial, i is 2... until in the n trial, i is n. 
# Then, I create vectors for the positions of the letters A and B. The package "which" allows me to locate where A is in the round i of the loop. 
# My results are presented as the probability of getting that sum (the number of times A and B are sitting together) over and i number of trials (in this case, 10000).

table_func <- function(trials) {
  people_vec <- LETTERS [1:19]
  sum <- 0
  for (i in 1:trials) {
    vec <- sample(people_vec)
    a_position <- which(vec == 'A')
    b_position <- which(vec == 'B')
    if (b_position == a_position + 1 | b_position == a_position - 1) {
      sum <- sum + 1
    }
    if (b_position == 20 & a_position == 1) {
      sum <- sum + 1
    }
    if (b_position == 1 & a_position == 20) {
      sum <- sum + 1
    }
    } 
  return(sum/trials)
}

table_func(10000) #set the number of trials to whatever number I want

## 2. Simulate some data from the following model for sample sizes n = 10, 20, . . . , 990, 1000.: yi|xi ∼ N (α + β1x1,i + β2x2,i, σ2)
## Draw the Xs from a standard Normal distribution. Set α = 1, β1 = 2, β2 = 3 and σ2 = 6. Then, write some code to compute the OLS estimates of the parameters. 

# First, I generate a function that generates a dataset that has information on the number of observations, the coefficients and the standard deviation. 
# I set the parameters so that n is the number of trials
# x1 and x2 are the two vectors of my equation, and they are built from a normal distribution random draw. 
# For that, I use the command rnorm, where the arguments are always: n(number of observations), mean, sd.
# I also build the dependent variable or Y vector, where the mean is based on the formula given in the prompt. 
# Again, n is the number of draws, the mean (which is the mean of the distribution) is generating data that aligns with how OLS predicts outcomes, and s2 is the sd.
# I am using cbind to create a matrix. For example:

b <- 1:10
c <- 1:2
d <- cbind(b,c)
d

# I want the results as a dataframe, and that is why I use the command return.

generate_dat <- function(n, b1, b2, s2) {
  for (i in 1:n) {
    x1 <- rnorm(n, 0, 1)
    x2 <- rnorm(n, 0, 1)
  }
  y <- rnorm(n, mean = b1 * x1 + b2 * x2, sd=s2)
  dat <- cbind(x1, x2, y)
  return(as.data.frame(dat))
}

# Now, I am asked to report the confidence intervals. Therefore, I need to create a function that provides this information.
# I am using a function that has the arguments: coefficient, standard error, and alpha.
# I use z-statistics to calculate the confidence intervals. To generate them, I use the qnorm, which refers to the quantile function of the normal distribution. 
# Then, I build the upper and lower ends of my confidence interval, based on the coefficient +/- the z.statistic times the standard error. 

conf_ints <- function(coef, se, alpha) {
  z.stat <- qnorm(alpha/2 , 0, 1)
  upper_int <- coef + z.stat*se
  lower_int <- coef - z.stat*se
  return(list(lower_int, upper_int))
}

# Now, I am asked to think of the different sample sizes I am being asked to work with. It's 10, 20, 30, etc. until 1000. To do this, I can use the package "seq".
# seq has three arguments: minimum value (10), last value (1000) and by (10). For example:

e <- seq(3, 30000, 20)
e

# I report the results in total.results, which is first an empty data frame.
# Here, I create another loop. This is not a function but could be one.
# for i in sizes, I generate four columns that are inside my data frame using all the functions previously generalted. I also use summary to get an easy access to the standard error. 
# b2_est is extracting the coefficinet from the model.
# mod$coeff has its own matrix of data, and I pick the third row, first column.
# CIs is the container holding the confidence interval function. 

sizes <- seq(10, 1000, 10)
total.results <- data.frame()
for (i in sizes) {
  dat <- generate_dat(n=i, b1=2, b2=3, s2 =6)
  mod <- summary(lm(data=dat, form=y ~ x1 + x2))
  b2_est <- mod$coefficients[3, 1]
  b2_se <- mod$coefficients[3, 2]
  CIs <- conf_ints(b2_est, b2_se, alpha=0.05)
  results <- data.frame(point_estimates = b2_est,
                        lower_interval = CIs[[1]],
                        upper_interval = CIs[[2]],
                        sample_size = i)
    total_results <- rbind(total_results, results)
}

plot(total_results$sample_size, total_results$point_estimates,
     type = "b", col = "red")
lines(total_results$sample_size, total_results$lower_interval,
      type = "b", col = "purple")
lines(total_results$sample_size, total_results$upper_interval,
      type = "b", col = "purple")
abhline(h=3)



