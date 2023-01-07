# 111550130 Mu-Ting Lee Sampling 111-11-14

## import library
library(ggplot2)
library(MASS)
library(moments)
library(dplyr)
library(readxl)
library(gridExtra)
## import data

weatherdata <- read_excel("weatherdata.xlsx")
temp <- weatherdata$Temperature

## density curve 
density <- density(temp)
df_dens <- data.frame(x = density$x, y = density$y)

## log-normal distribution
vir <- seq(10, 40, 0.1)
Normal <- fitdistr(temp, "normal")
Normal_mean = Normal$estimate[1]
Normal_sd = Normal$estimate[2]
dNormal = dnorm(vir, mean = Normal_mean, sd = Normal_sd)

## Draw the fit-plot
fit <- data.frame(x = vir, norm = dNormal)
df <- data.frame(x = temp)
colors = c("density" = "red", "log-normal" = "skyblue")
ggplot() + 
  scale_x_continuous(breaks = seq(10, 40, 10)) + 
  scale_color_manual(values = colors) + 
  geom_histogram(data = df, aes(x, ..density..), binwidth = 0.5, fill = "black", color = "white") + 
  geom_line(data = df_dens, aes(x, y, color = "density"), size = 0.5) + 
  geom_line(data = fit, aes(x, norm , color = "log-normal"), size = 0.5) +
  labs(title = "June 01-Sept. 30 2020 Temperature", 
       x = "Temperature", y = "Density", color = "Legend")

## sampling parameters
len <- length(temp)

## random sampling n = 10, times = 1000
n <- 10
times <- 1000
mean_10 <- 0.0
for(i in 1:times){
    idx <- sample(1:len, n)
    mean_10[i] <- mean(temp[idx])
}
mean_10 <- as.data.frame(mean_10)

p1 <- ggplot(data = mean_10, aes(mean_10)) + 
  geom_histogram(bins = 30) + 
  labs(x = "sample mean, random sampling n = 10, 1000 times") + 
  coord_cartesian(xlim = c(15, 35))


## random sampling n = 50, times = 1000
n <- 50
times <- 1000
mean_50 <- 0.0
for(i in 1:times){
  idx <- sample(1:len, n)
  mean_50[i] <- mean(temp[idx])
}
mean_50 <- as.data.frame(mean_50)

p2 <- ggplot(data = mean_50, aes(mean_50)) + 
  geom_histogram(bins = 30) + 
  labs(x = "sample mean, random sampling n = 50, 1000 times") + 
  coord_cartesian(xlim = c(15, 35))

## random sampling n = 10, times = 1000
n <- 100
times <- 1000
mean_100 <- 0.0
for(i in 1:times){
  idx <- sample(1:len, n)
  mean_100[i] <- mean(temp[idx])
}
mean_100 <- as.data.frame(mean_100)

p3 <- ggplot(data = mean_100, aes(mean_100)) + 
  geom_histogram(bins = 30) + 
  labs(x = "sample mean, random sampling n = 100, 1000 times") + 
  coord_cartesian(xlim = c(15, 35))


## combine plots
grid.arrange(p1, p2, p3, nrow = 3)

## Skewness and Kurtosis Analysis
sk <- skewness(temp)
kt <- kurtosis(temp)

sk  # 0.5073322
kt  # 2.122686

sk_10 <- skewness(mean_10)
kt_10 <- kurtosis(mean_10)
sk_10 #0.2096557 
kt_10 #2.828202 

sk_50 <- skewness(mean_50)
kt_50 <- kurtosis(mean_50)
sk_50 #0.07669248 
kt_50 #2.943173 

sk_100 <- skewness(mean_100)
kt_100 <- kurtosis(mean_100)
sk_100 #-0.0480247 
kt_100 #2.975942 
