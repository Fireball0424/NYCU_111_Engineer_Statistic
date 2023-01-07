# File: R_descriptive_b.R
# Course: Ch5.R.Descriptive_Statistics
# Section: (0)read datasets from R
#          (1)histogram plot with count
#          (2)histogram plot with density + density curve
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-10-24

# (0)
# datasets: cars
data()
data(cars)
#?cars
names(cars)

# (1)
# histogram plot
library(ggplot2)
n <- length(cars$dist)
?geom_histogram
?hist
hist(cars$dist, breaks = 20, plot = TRUE)
hisp <- hist(cars$dist, breaks = 20, plot = FALSE)
hisp$breaks
# breaks option can be replaced by bins
p1 <- ggplot(data = cars, aes(dist))+
  geom_histogram(  fill = "red"
                 , alpha = 0.5
                 #, bins = 20)+
                 , breaks = hisp$breaks)+
  labs(title = "Histogram of distance to stop"
       , x = "Distance [ft]", y = "Number of count")+
  theme_bw()

# Y-axis is density scale: ..density..
?density
# width, bw, adjust
# check density$bw
density <- density(cars$dist,width = 20)
density$bw
data.density <- data.frame(x = density$x, y = density$y)
p2 <- ggplot(data = cars, aes(dist,..density..))+
  geom_histogram(fill = "blue"
                 , alpha = 0.5
                 , breaks = hisp$breaks)+
  geom_line(data = data.density
            , aes(x,y)
            , color = "red"
            , size = 1)+
  labs(title = "Histogram of distance to stop"
       , x = "Distance [ft]", y = "Density")+
  theme_bw()

#install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1,p2, nrow = 2)
