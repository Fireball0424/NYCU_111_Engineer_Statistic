# File: R_regression_b.R
# Course: Ch9.R.RegressionAnalysis
# Section: power transformation
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-11-28

# load data
library(rio)
getwd()
setwd("C:/111_1_Engineering_Statistics/Ch9.R.Regression_Analysis")
data <- import("data_power.xlsx")
str(data)  # look the structure of data

# scatter plots
library(ggplot2)
ggplot(data = data, aes(frytime,moisture))+
  geom_point()+
  labs(title = "Scatter plots example",
       x = "Fry time, sec",
       y = "Moisture, %")

data$frytime <- log(data$frytime)
data$moisture <- log(data$moisture)

ggplot(data = data, aes(frytime,moisture))+
  geom_point()+
  labs(title = "Scatter plots example",
       x = "ln(Fry time), ln(sec)",
       y = "ln(Moisture), ln(%)")

# start to fitting line
model <- lm(moisture~frytime, data = data)
summary(model) 
e <- exp(1)
a <- e^model$coefficients[1]
b <- model$coefficients[2]
a
b

# re-calculate the R square
SSE <- 0
SSTo <- 0 
data <- import("data_power.xlsx")
ymean <- mean(data$moisture)
n <- length(data$moisture)
for (i in 1:n) {
  ypre <- a*data$frytime[i]^b
  SSE = SSE + (data$moisture[i]-ypre)^2
  SSTo = SSTo + (data$moisture[i]-ymean)^2
}
se.curve <- sqrt(SSE/(n-2))
se.curve
r2.curve <- 1 - SSE/SSTo
r2.curve

sub.text <- expression(paste("r"^"2","=0.932,"," Moisture = ","103.38 x ","frytime"^"-1.049"))

# predict curve line

pre.x <- seq(5,60,0.01)
pre.y <- a*pre.x^b
curve <- data.frame(x=pre.x,y=pre.y)
data <- import("data_power.xlsx")
str(data)  # look the structure of data
ggplot(data = data, aes(frytime,moisture))+
  geom_point()+
  geom_line(data = curve, aes(x,y),
            linetype = 2)+
  labs(title = "Scatter plots example",
       subtitle = sub.text,
       caption = "Power Transformations",
       x = "Fry time, sec",
       y = "Moisture, %")

