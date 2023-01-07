# File: R_testing_a.R
# Course: Ch11.R.Hypothesis_Testing
###------t test: t.test()
# Section: random sampling n = 20 from 1:100 integers
#          (1) H0: = 50.0
#          (2) H0: >= 30.0
#          (3) H0: <= 30.0
#          with significant level (alpha) = 5%
###------power compute
#          (4) power.t.test()
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-12-12

# random sampling,
n <- 20
x <- sample(1:100,size = 20, replace = FALSE)
# t test for (1) H0 = 50.0
# two-tailed test 
mu <- mean(x)  # mean 
s <- sd(x)     # standard deviation
t <- (mu-50)/(s/sqrt(n))
p <- pt(t,df = n - 1)
p <- 1-p
p <- 2.0*p
t.test(x
       , mu = 50
       , alternative = "two.sided"
       , conf.level = 0.95)

# t test for (2) H0 >= 30.0
# lower-tailed test
t <- (mu-30)/(s/sqrt(n))
p <- pt(t,df = n - 1)

t.test(x
       , mu = 30
       , alternative = "less"
       , conf.level = 0.95)

# t test for (3) H0 <= 30.0
# Upper-tailed test
t <- (mu-30)/(s/sqrt(n))
p <- pt(t,df = n - 1)
p <- 1 - p 
t.test(x
       , mu = 30
       , alternative = "greater"
       , conf.level = 0.95)
# (4)
# find power & determine the sample number
power.t.test(n
             , delta = mu-30
             , sd = s
             , sig.level = 0.05
             , power = NULL
             , type = "one.sample"
             , alternative = "one.sided")
# what's sampling number can increase power to be 90%
power.t.test(n = NULL
             , delta = mu-30
             , sd = s
             , sig.level = 0.05
             , power = 0.99
             , type = "one.sample"
             , alternative = "one.sided")

