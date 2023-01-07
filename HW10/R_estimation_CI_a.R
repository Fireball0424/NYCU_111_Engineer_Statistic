# File: R_estimation_CI_a.R
# Course: Ch10.R.Estimation_and_Confidence_Interval
# Section: (1) large-sample confidence interval, z-test
#          (2) small-sample confidence interval, t-test
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-12-05

# create population data
p.data <- c(rpois(50000,45))
true.mean <- mean(p.data)
library(ggplot2)
p.plot <- data.frame(x=p.data)
ggplot(data = p.plot, aes(x))+
  geom_histogram(bins = 20)+
  labs(title = "Population data",
       x = "Numeric data",
       y = "Count")

library(moments)
skewness(p.data)

# (1)- large-sample
# take a sample of 1000
set.seed(1)
s.data <- sample(p.data,size = 1000)
s.plot <- data.frame(x=s.data)
ggplot(data = s.plot, aes(x))+
  geom_histogram(bins = 20)+
  labs(title = "Sampled data",
       x = "Numeric data",
       y = "Count")

skewness(s.data)

# make a point estimation of the mean
sample.mean <- mean(s.data)

# check difference between estimate and population mean
sample.mean
sample.mean - true.mean


# the central limit theorem
set.seed(10)
point.estimation <- c() # create an empty vector to hold results 

num.sample <- 200 # to make 100 point.estimation

for (i in 1:num.sample){
  s.data <- sample(p.data,size = 1000)
  point.estimation[i] <- mean(s.data)
}
point.plot <- data.frame(x=point.estimation)
ggplot(data = point.plot, aes(x))+
  geom_histogram()+
  labs(title = "Sampled data",
       x = "Numeric data",
       y = "Count")

skewness(point.estimation) # check normal distribution

mean(point.estimation)
mean(point.estimation) - true.mean

# Confidence intervals
# Bound of error : (z-critical value)*population_sigma/sqrt(n)
set.seed(100)
num.sample = 1000
s.data <- sample(p.data,size = num.sample)

z.star <- qnorm(0.975)  # z-critical value for a 95% CL
p.sd <- sd(p.data)
B <- z.star*p.sd/sqrt(num.sample)

confidence.interval <- c(mean(s.data)-B,
                         mean(s.data)+B)
confidence.interval  # check confidence interval

install.packages("BSDA")
library(BSDA)
z.test(s.data, sigma.x = p.sd, conf.level = 0.95)

#(2)-small-sample
set.seed(200)
num.sample = 25
s.data <- sample(p.data,size = num.sample)
sample.mean <- mean(s.data)
sample.mean  # get the sample mean
# get the t-critical value
t.star <- qt(0.975, df = num.sample - 1)   
# Bound of error : (t-critical value)*sample_sigma/sqrt(n)
s.sd <- sd(s.data)
B <- t.star*s.sd/sqrt(num.sample)
confidence.interval <- c(mean(s.data)-B,
                         mean(s.data)+B)
confidence.interval  # check confidence interval

t.test(s.data,conf.level = 0.95)
