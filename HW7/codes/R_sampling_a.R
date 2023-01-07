# File: R_sampling_a.R
# Course: Ch7.R.Sampling
# Section: random sampling
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-11-07

# --------Sample------------------------- 
# From the integer 1-10, pick up 5 number
sample(x = 1:100, size = 5)
# Wtih replacement
sample(x = 1:10, size = 10
       , replace = TRUE)
# Sampling the character
sample(x = c("T","F"), size = 10
       , replace = TRUE)
# Setting the probability
sample(x = c("T","F"), size = 10
       , replace = TRUE
       , prob = c(0.9,0.1))

# Generating same random data
set.seed(1)
sample(x = 1:100, size = 10)
set.seed(12)
sample(x = 1:100, size = 10)
set.seed(12)
sample(x = 1:100, size = 10)

# Five samples from uniform distribution with bounds at 0 and 1
runif(n = 85, min = 0, max = 1)



