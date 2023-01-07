# File: R_sampling_c1.R
# Course: Ch7.R.Sampling
# Section: fitting data with density functions: log-norm, weibull 

# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-11-07

library(rio)
getwd()
setwd("C:/111_1_Engineering_Statistics/Ch7.R.Sampling")
d <- import("weatherdata.xlsx")
names(d)
#install.packages("dplyr")
# if you meet trouble:　
# Error in select(., ..) : unused argument
# please restarting your R session
# Ctrl + Shift + F10
# and running your code again
library(dplyr)
d.rain <- d %>%
  select(Rain) %>%   # select the Rainfall data
  filter(Rain > 0.0) # remove the rainfall = 0
str(d.rain)

library(MASS)
?fitdistr
# log-normal
fit.lnorm <- fitdistr(d.rain$Rain,densfun = "lognormal")
mean <- fit.lnorm$estimate[1]
sd <- fit.lnorm$estimate[2]
x <- seq(-1,10,0.1)
?dlnorm
y <- dlnorm(x, meanlog = mean, sdlog = sd)
d.lnorm <- data.frame(x = x, y = y, label = "lnorm") 
# label for plot

# weibull
fit.wei <- fitdistr(d.rain$Rain, densfun = "weibull")
shape <- fit.wei$estimate[1]
scale <- fit.wei$estimate[2]
y <- dweibull(x, shape = shape, scale = scale)
d.wei <- data.frame(x = x, y = y, label = "weibull")

# density curve
density <- density(d.rain$Rain)
data.density <- data.frame(x = density$x
                           , y = density$y
                           , label = "density curve")

data.plot <- rbind.data.frame(d.lnorm, d.wei, data.density)

?geom_histogram
library(ggplot2)

ggplot(data = d.rain, aes(Rain,..density..)) +
  geom_histogram(binwidth = 0.2,
                 fill = "black",
                 color = "white",
                 size =0.1,
                 alpha = 0.8)+
  geom_line(data = data.plot
            , aes(x,y, color = label) # define color by label 
            , size = 0.6)+
  labs(title = "June 01-Sept. 30 2020 Rainfall",
     x = "Rainfall [mm]",
     y = "Density")+
  scale_color_discrete(name = "Lines")+
  theme_bw()

library(moments)
skewness(d.rain)
kurtosis(d.rain)
