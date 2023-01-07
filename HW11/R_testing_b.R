# File: R_testing_b.R
# Course: Ch11.R.Hypothesis_Testing
# Section: Tests Concerning a Difference Between two Means: 
#          (1) data normality test, shapiro.test()
#          (2) make a box plot to see the distribution properties of two data
#          (3) H0: mu1 - mu2 = 0
#          with significant level (alpha) = 1%,5%
#
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2021-12-12

# import data
library(rio)
getwd()
setwd("D:/110_1_Engineering_Statistics/Ch11.R.Testing_Hypotheses")
d <- import("data.xlsx")
index.no.fu <- which(d$treament == "nofusion")
index.fu <- which(d$treament == "fused")
No.fu <- d$strength[index.no.fu]
fu <- d$strength[index.fu]  # save data if is.na = FALSE
# for no fusion
n1 <- length(No.fu)
mu1 <- mean(No.fu)
s1 <- sd(No.fu)
se1 <- s1/sqrt(n1)
# for fused
n2 <- length(fu)
mu2 <- mean(fu)
s2 <- sd(fu)
se2 <- s2/sqrt(n2)

# (1) data normality test, shapiro.test()
# H0: the hypothesis of normality
shapiro.test(No.fu)
shapiro.test(fu)

# (2) make a box plot to see the distribution properties of two data
library(ggplot2)
ggplot(data = d, aes(treament,strength))+
        geom_boxplot()+
        coord_flip()+
        geom_point(aes(color = strength)
                   ,alpha = 0.5)+
        labs(title = "Boxplot",
             y = "Strength",
             x = "Treament")
        
# (3) H0: mu1 - mu2 = 0; Ha: mu1 - mu2 < 0
var.test(No.fu,fu,ratio = 1)  # F-test
t.test(No.fu,fu
       , alternative = "less"
       , var.equal = FALSE
       , conf.level = 0.95)

t.test(No.fu,fu
       , alternative = "two.sided"
       , var.equal = FALSE
       , conf.level = 0.95)

