# File: R_estimation_CI_b.R
# Course: Ch10.R.Estimation_and_Confidence_Interval
# Section: large-sample CI for proportion, pi
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-12-05

# 
p <- 0.333
n <- 48
z.star <- qnorm(0.975)
B <- z.star * sqrt(p*(1-p)/n + z.star^2/(4*n*n))
tmp <- 1 + (z.star^2)/n
confidence.interval <- c((p + (z.star^2)/(2*n) - B)/tmp,
                         (p + (z.star^2)/(2*n) + B)/tmp)
confidence.interval

?prop.test
# if correct = TRUE: Yates' continuity correction should be applied 
prop.test(x = 16, n = 48
          , conf.level = 0.95
          , correct = FALSE)  # FALSE used here
