# File: R_descriptive_d.R
# Course: Ch5.R.Descriptive_Statistics
# Section: IQR in normal distribution
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-10-24

# Lower quartile Q25
q25 <- qnorm(0.25,mean = 0, sd = 1)
# Upper quartile Q75
q75 <- qnorm(0.75,mean = 0, sd = 1)
IQR <- q75-q25

cat("IQR in normal distribution: ", IQR)

