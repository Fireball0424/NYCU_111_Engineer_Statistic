##111550130 李慕庭 ch5 Descriptive Oct/24/2022

##import packages
library(ggplot2)
library(moments)

## create data frame
df <- data.frame(measurement_data = 
                   c(37.0, 37.5, 38.1, 40.0, 40.2,
                     40.8, 41.0, 42.0, 43.1, 43.9,
                     44.1, 44.6, 45.0, 46.1, 47.0,
                     50.2, 55.0, 56.0, 57.0, 58.0,
                     62.0, 64.3, 68.8, 70.1, 74.5))

## (1) construct a histogram with the density curve
hist_plot <- hist(x = df$measurement_data, breaks = 5, plot = FALSE)

density <- density(x = df$measurement_data)
df_density <- data.frame(x = density$x, y = density$y)

plot1 <- ggplot(data = df, aes(x = measurement_data, y = ..density..))+
          geom_histogram(breaks = hist_plot$breaks, fill = "blue", alpha = 0.5) + 
          geom_line(data = df_density, mapping = aes(x, y), size = 1, color = "red")+
          labs(title = "Histogram and Density curve of data", 
               x = "Measurement data [MPa]", y = "Density")
#plot1

##(2) skewness and kurtosis
skewness(df$measurement_data)
kurtosis(df$measurement_data)

##(3) Construct a boxplot display of the data.
plot2 <- ggplot(data = df, aes(x = measurement_data)) + 
  geom_boxplot(coef = 1.0, 
               outlier.color = "red", 
               outlier.shape = 8, 
               outlier.stroke = 1,
               outlier.size = 2) + 
  scale_y_discrete() + 
  labs(title = "Boxplot of data", x = "Measurement data [MPa]")

## print the graph 
grid.arrange(plot1, plot2, nrow = 2)
