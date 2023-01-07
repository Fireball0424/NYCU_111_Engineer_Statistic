# File: R_probability_b.R
# Course: Ch8.R.Probability
# Section: Dice games
#          create function in R
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-11-21

dice.game <- function(n){
  num <- sample(x=1:6, size = n, replace = TRUE)
  # start to compute the count for each numbers
  count <- 0
  x <- 0
  for (i in 1:6){
    count[i] <- 0
    x[i] <- i
    for (j in 1:n){
      if(num[j] == i) count[i]=count[i]+1
    }
  }
  # compute probability for dice number 1:6
  pro <- count/n
  # theoretical probability pro.t
  pro.t <- 1/6
  str <- as.character(n)
  title <- paste("Testing of Dice-game probability-n=",str)
  lpx <- c(1,6)
  lpy <- c(pro.t,pro.t)
  data.line <- data.frame(x=lpx, y=lpy)
  library(ggplot2)
  data <- data.frame(x=x, y=pro)
  ggplot(data = data, aes(x,y))+
    geom_point()+
    labs(title = title,
         x = "Dice Number",
         y = "Probability")+
    geom_line(data = data.line, aes(x,y),
              linetype = 2,
              color = "black",
              size = 1.)+
    ylim(0,0.5)+
    scale_x_continuous(breaks = seq(1, 6, by = 1))+
    theme_bw()
}

dice.game(100)

