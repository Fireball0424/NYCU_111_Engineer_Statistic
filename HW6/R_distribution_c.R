# File: R_distribution_c.R
# Course: Ch6.R.Distribution
# Section: log-normal distribution
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-10-31

mean <- 0.353
sd <- 0.754
x <- seq(0,10,0.001)
lnor <- dlnorm(x, mean = mean, sd = sd)
data <- data.frame(x = x, y = lnor)
library(ggplot2)
ggplot(data = data, aes(x,y))+
  geom_line()+
  geom_point(size = 0.5)+
  labs(title = "Log normal distribution",
       x = "Maximum pit depth",
       y = "Probability")

p <- plnorm(2,mean = mean, sd = sd) - plnorm(1,mean = mean, sd = sd)

