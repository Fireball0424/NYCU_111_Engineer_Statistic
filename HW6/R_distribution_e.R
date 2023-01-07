# File: R_distribution_e.R
# Course: Ch6.R.Distribution
# Section: fitting distribution
#          (1) fit data to likely distribution
#          (2) check the fit of data: QQplot
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-10-31

# load data
d <- c(30.86, 37.68, 39.04, 42.78, 42.89, 42.89, 45.05, 47.08, 47.08, 48.79
       , 48.79, 52.56, 52.56, 54.8, 55.17, 56.31, 59.94)
data <- data.frame(x =d)
density <- density(d) # find density curve
data.density <- data.frame(x = density$x, y = density$y)



#(1)
#install.packages("MASS")
library(MASS)
# fit function by using weibull function
fit.weibull <- fitdistr(d,"weibull")
shape <- fit.weibull$estimate[1] #alpha
scale <- fit.weibull$estimate[2] #beta
x <- seq(20,70,0.1)
d.weibull <- dweibull(x, shape = shape, scale = scale)
# normal
fit.normal <- fitdistr(d,"normal")
mean <- fit.normal$estimate[1]
sd <- fit.normal$estimate[2]
d.normal <- dnorm(x, mean = mean, sd = sd)

# fitting lines
fit <- data.frame(x = x, wei = d.weibull, nor = d.normal)

library(ggplot2)
ggplot(data = data ,aes(x,..density..))+
  xlab("Ratio between length and diameter")+
  scale_x_continuous(breaks = seq(20,70,10))+
  geom_histogram(binwidth = 6
                 , fill = "blue", color = "gray"
                 , alpha = 0.5)+
  geom_line(data = data.density, aes(x,y),
            color = "black",
            size =1.0)+
  geom_line(data = fit, aes(x,wei), # weibull
            color = "red",
            size =1.0)+
  geom_line(data = fit, aes(x,nor), # normal
            color = "black",
            size =1.0,
            linetype = 2)+
  theme_bw()


#(2) QQplot for normal distribution
ggplot()+
  aes(sample = d)+
  stat_qq(distribution = qnorm)+
  stat_qq_line(line.p = c(0.25, 0.75), col = "blue")+
  labs(title = "QQplot",
       x = "Theoretical Normal Distribution Quantile",
       y = "Observed Data")

library(moments)
sk <- skewness(d)
sk
kt <- kurtosis(d)
kt


