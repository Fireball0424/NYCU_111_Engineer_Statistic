# File: R_regression_c.R
# Course: Ch9.R.RegressionAnalysis
# Section: polynomial function
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-11-28

# load data
library(rio)
getwd()
setwd("D:/20220506/courses/2022/111_1_Engineering_Statistics/Ch9.R.Regression_Analysis")
data <- import("data_conc.xlsx")
str(data)  # look the structure of data

cor(data$time,data$concentration)

# scatter plots
library(ggplot2)
ggplot(data = data, aes(time,concentration))+
  geom_point()+
  labs(title = "Scatter plots example",
       x = "Time, day",
       y = "Concentration, %")

# start to fitting line
model <- lm(concentration ~ time + I(time^2), data = data)
summary(model)
a <- model$coefficients[1]
b1 <- model$coefficients[2]
b2 <- model$coefficients[3]
sub.text <- expression(paste("r"^"2","=0.8948,"
                             ," Concentration = ","84.48 + "," -15.875 x time ",
                             " +1.768 x time"^"2"))

# predict curve line

pre.x <- seq(0,10,0.01)
pre.y <- a + pre.x*b1 + b2*pre.x*pre.x
curve <- data.frame(x=pre.x,y=pre.y)
ggplot(data = data, aes(time,concentration))+
  geom_point()+
  geom_line(data = curve, aes(x,y),
            linetype = 2)+
  labs(title = "Scatter plots example",
       subtitle = sub.text,
       caption = "Polynomial function",
       x = "Fry time, sec",
       y = "Moisture, %")

