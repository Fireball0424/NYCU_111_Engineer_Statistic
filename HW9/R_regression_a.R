# File: R_regression_a.R
# Course: Ch9.R.RegressionAnalysis
# Section: scatterplot, linear coefficient, fitting line
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-11-28

# load data
library(rio)
getwd()
setwd("D:/20220506/courses/2022/111_1_Engineering_Statistics/Ch9.R.Regression_Analysis")
data <- import("data.xlsx")
str(data)  # look the structure of data

# scatter plots
err<- paste("Ocular Surface Area",", cm"^"2")
y.lab <- expression(paste("Ocular Surface Area",", cm"^"2"))
y.lab
library(ggplot2)
#?geom_point
ggplot(data = data, aes(Width,OSA))+
  geom_point()+
  labs(title = "Scatter plots example",
       x = "Distance between eyes, cm",
       y = y.lab)+
  ylim(0,6)+
  xlim(0,2)

?geom_text
# Pearson's correlation coefficient
n <- length(data$No)
xx <- 0.
x2 <- 0.
yy <- 0.
y2 <- 0.
xy <- 0.
for (i in 1:n){
  xx <- xx + data$Width[i]*data$Width[i]
  x2 <- x2 + data$Width[i]
  yy <- yy + data$OSA[i]*data$OSA[i]
  y2 <- y2 + data$OSA[i]
  xy <- xy + data$Width[i]*data$OSA[i]
}
xy2 <- (x2*y2)/n
x2 <- (x2^2.0)/n
y2 <- (y2^2.0)/n
Sxx <- xx-x2
Syy <- yy-y2
Sxy <- xy-xy2
r <- Sxy/(sqrt(Sxx)*sqrt(Syy))
# using cor() function
r.cor <- cor(data$Width,data$OSA,method = "pearson")
rt <- as.character(round(r.cor,2))
r.text <- paste("r: ",rt)

# start to find a straight line by LS method
# y = a + bx
model <- lm(OSA ~ Width, data = data)
summary(model)
a <- model$coefficients[1]
b <- model$coefficients[2]
anova <- anova(model)
anova
SSE <- anova$`Sum Sq`[2]
SSR <- anova$`Sum Sq`[1]
SSTo <- SSE + SSR
r.square <- 1 - SSE/SSTo
se <- sqrt(SSE/(n-2))

# fitting without intercept
model.2 <- lm(OSA ~ Width + 0, data = data)
b2 <- model.2$coefficients[1]
summary(model.2)

# predict the regression line
pre.x <- seq(0.0,2.0,0.01)
pre.y <- a + b*pre.x
pre.line <- data.frame(x=pre.x,y=pre.y)

sub.text <- expression(paste("r"^"2","=0.9373"," ,OSA = ","-0.398 + 3.08 x Width"))


ggplot(data = data, aes(Width,OSA))+
  geom_point()+
  geom_line(data = pre.line, aes(x,y))+
  geom_abline(slope = b, intercept = a)+
  geom_abline(slope = b2, intercept = 0.0, color = "red")+
  geom_text(x = 0.6, y = 5, 
            label = r.text, color = "black")+
  labs(title = "Scatter plots example",
       subtitle = sub.text,
       caption = "lines without(red) and with(black) intercept",
       x = "Distance between eyes, cm",
       y = y.lab)+
  ylim(0,6)+
  xlim(0,2)


