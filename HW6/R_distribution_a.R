# File: R_distribution_a.R
# Course: Ch6.R.Distribution
# Section: (1-1) standard normal distribution
#          (1-2) general normal distribution
#          (2) probability & quantile
#          (3) random number
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-10-31

# (1)
# standard normal distribution
# mean = 0, sd = 1
# x data
xtmp <- seq(-20,20,0.01)
x <- rep(xtmp,3)
# y data
y1 <- dnorm(xtmp, mean = 0, sd = 1) # standard ND
y2 <- dnorm(xtmp, mean = 5, sd = 1) 
y3 <- dnorm(xtmp, mean = 0, sd =3)
yy <- cbind(y1,y2,y3)  # 4001 x 3  # rbind
y <- matrix(yy,ncol = 1) # 12003 x 1
# label data for line color 
g1 <- rep(1,4001)
g2 <- rep(2,4001)
g3 <- rep(3,4001)
gg <- cbind(g1,g2,g3)
group <- factor(matrix(gg,ncol = 1))
data <- data.frame(x = x, y = y, label = group)
str(data)

library(ggplot2) # ggtext
ggplot(data = data, aes(x = x, y = y, color = label))+
  geom_line(size = 1)+
  labs(title = "Normal Distribution (ND)",
       x = "x variable",
       y = "Probability")+
  scale_color_discrete(name = "Parameter of ND",
                       labels = c("mean:0 sd:1",
                                  "mean:5 sd:1",
                                  "mean:0 sd:3"))+
  theme_get()

#(2) probability & quantile
pnorm(0, mean = 0, sd = 1)
pnorm(5, mean = 5, sd = 1)
pnorm(0, mean = 0, sd = 3)
qnorm(0.5, mean = 5, sd = 1)

#(3) random number of standard normal distribution
# 1000 data points
random <- rnorm(1000, mean = 0, sd = 1)
summary(random)
data <- data.frame(x = random)
# density curve
ggplot(data = data, aes(x,..density..))+
  geom_histogram(bins = 60
                 , fill = "blue"
                 , alpha = 0.5)+
  labs(title = "Histogram of random number (data point: 1000)",
       x = "x variable",
       y = "Density")



