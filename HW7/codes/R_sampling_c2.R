# File: R_sampling_c2.R
# Course: Ch7.R.Sampling
# Section: checking the sampling distributions with 
#          different sample number 10, 50, 100
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-11-07

library(rio)
getwd()
setwd("C:/111_1_Engineering_Statistics/Ch7.R.Sampling")
d <- import("weatherdata.xlsx")
names(d)
#install.packages("dplyr")
library(dplyr)
d.rain <- d %>%
  select(Rain) %>%
  filter(Rain > 0.0) # ctrl + shift + F10

summary(d.rain)

n <- 10
nt <- 1000
np <- length(d.rain$Rain)
# do loop for sampling nt times, n data per one time
d.mean.10 <- 0.0
i <- 0
for (i in 1:nt) {
  d.index <- sample(1:np,n)
  d.sample <- d.rain$Rain[d.index]
  d.mean.10[i] <- mean(d.sample)
}
d.mean.10 <- as.data.frame(d.mean.10)

n <- 50
nt <- 1000
np <- length(d.rain$Rain)
# do loop for sampling nt times, n data per one time
d.mean.50 <- 0.0
i <- 0
for (i in 1:nt) {
  d.index <- sample(1:np,n)
  d.sample <- d.rain$Rain[d.index]
  d.mean.50[i] <- mean(d.sample)
}
d.mean.50 <- as.data.frame(d.mean.50)

n <- 100
nt <- 1000
np <- length(d.rain$Rain)
# do loop for sampling nt times, n data per one time
d.mean.100 <- 0.0
i <- 0
for (i in 1:nt) {
  d.index <- sample(1:np,n)
  d.sample <- d.rain$Rain[d.index]
  d.mean.100[i] <- mean(d.sample)
}
d.mean.100 <- as.data.frame(d.mean.100)


# plot histogram to check the shape, is it bell shape?
library(ggplot2)
p1 <- ggplot(data = d.mean.10, aes(d.mean.10))+
  geom_histogram(bins = 30)+
  coord_cartesian(xlim = c(0.1,2.0))+
  labs(x = "sample mean, random sampling n = 10, 1000 times")
p2 <- ggplot(data = d.mean.50, aes(d.mean.50))+
  geom_histogram(bins = 30)+
  coord_cartesian(xlim = c(0.1,2.0))+
  labs(x = "sample mean, random sampling n = 50, 1000 times")
p3 <- ggplot(data = d.mean.100, aes(d.mean.100))+
  geom_histogram(bins = 30)+
  coord_cartesian(xlim = c(0.1,2.0))+
  labs(x = "sample mean, random sampling n = 100, 1000 times")

library(gridExtra)
grid.arrange(p1,p2,p3,nrow =3)
