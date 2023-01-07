# 10/17 111550130 李慕庭 Visualization

#import ggplot, lubridate, rio and gridextra
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("gridExtra")
library(ggplot2)
library(rio)
library(lubridate)
library(gridExtra)

#import dataset
df <- import("weatherdata.xlsx")
#head(df)
#names(df)

#convert character to date 
date <- df$Date
date_format <- mdy_hms(date)

df1 <- data.frame(Time = date_format, RH = df$RH)
df2 <- data.frame(Time = date_format, Rain = df$Rain)

#English version
Sys.setlocale("LC_ALL", "English")

#draw the graph 1 
p1 <- ggplot(data = df1, mapping = aes(x = Time, y = RH))+
  geom_point(color = "black", alpha = 0.5, size = 0.5, shape = 15)+
  geom_smooth(color = "blue")+
  ylim(c(30, 100))+
  labs(title = "Relative Humidity June-Oct 2020",
       x = "Date-Time",
       y = "RH [%]")

#draw the graph 2 
p2 <- ggplot(data = df2, mapping = aes(x = Time, y = Rain))+
  geom_bar(stat = "identity", color = "blue")+
  labs(title = "Precipitation June-Oct 2020", 
      x = "Date-Time", 
      y = "Precipitation [mm]")

#print two graphs
grid.arrange(p1, p2, nrow = 2)

#export the graph
#ggsave("111550130-Visualization.png")
