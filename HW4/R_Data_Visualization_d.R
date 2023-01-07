# File: R_Data_visualization_d.R
# Course: Ch4.R.Data.Visualization
# Section: (0)read timeseries data in R
#          (1)convert character date/time to POSIX
#          (2)bar plot for RH, relative humidity
#          (3)two plots on a page
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-10-17

#(0)
# read the data into R
library(rio)
getwd()
setwd("C:/111_1_Engineering_Statistics/Ch4.R.Visualization")
xlsx <- import("weatherdata.xlsx")
View(xlsx)
str(xlsx)
head(xlsx)
names(xlsx)

# (1) convert character to date format
install.packages("lubridate")
library(lubridate)
date <- xlsx$Date
dt <- mdy_hms(date)
str(dt)

# (2) relative humidity RH[%] & barplot
data <- data.frame(time = dt
                   , RH = xlsx$RH)
str(data)
library(ggplot2)
Sys.setlocale("LC_ALL", "English")
ggplot(data = data, aes(time, RH))+
  geom_bar(stat = "identity",color = "black")+
  labs(title = "RH June-Oct 2020"
       , x = "Date-Time"
       , y = "RH [%]")

#(3) two plots on a page
# install package "gridExtra"
install.packages("gridExtra")
library(gridExtra)


p1 <- ggplot(data = data, aes(time, RH))+
  geom_bar(stat = "identity",color = "black")+
  labs(title = "first plot"
       , x = "Date-Time"
       , y = "RH [%]")

p2 <- ggplot(data = data, aes(time, RH))+
  geom_bar(stat = "identity", color = "blue")+
  labs(title = "second plot"
       , x = "Date-Time"
       , y = "RH [%]")

grid.arrange(p1,p2,nrow = 2)
