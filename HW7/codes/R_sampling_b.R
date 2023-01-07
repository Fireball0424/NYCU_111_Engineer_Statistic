# File: R_sampling_b.R
# Course: Ch7.R.Sampling
# Section: statistics from random sampling distribution
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-11-07

# Random Sampling from Normal Distribution with mean of 50 and SD of 2
# n = 25 & 1000 times
# (1) theoretical numbers from normal distribution
x <- seq(44,56,0.1)
d <- dnorm(x,mean = 50, sd = 2)
d.norm <- data.frame(x = x, y = d, label = "normal")
library(ggplot2)
p1 <- ggplot(data = d.norm, aes(x,y))+
  geom_line()+
  scale_x_continuous(breaks = seq(44, 56, by = 1))+
  labs(title = "Normal Distribution of mean 50 and sd 2",
       x = "Number variable",
       y = "Density")

# (2) n = 25, repeatably sampling 1,000
# do loop
i <- 0
x.mean <- 0
# you can change the sample size n 
rnorm(2, mean = 50, sd = 2)
for (i in 1:1000) {
  x.mean[i] <- mean(rnorm(2, mean = 50, sd = 2))
}
d.mean <- data.frame(x = x.mean)
p2 <- ggplot(data = d.mean, aes(x,..density..))+
  geom_histogram()+
  coord_cartesian(xlim = c(44,56))+
  scale_x_continuous(breaks = seq(44, 56, by = 1))+
  labs(title = " Sample size n = 2 ",
       x = "Sample mean",
       y = "Density")

library(gridExtra)
grid.arrange(p1,p2,nrow = 2)


