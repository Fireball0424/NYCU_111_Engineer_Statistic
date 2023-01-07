#111550130 Mu-Ting Lee ch10 estimation 

## import library
library(BSDA)
library(ggplot2)

## constant
mu <- 50
sigma <- 10
num.sample = 20

## mean
estimation <- c()

## t critical value
t.star <- c()

## bound of error
error <- c()

## estimation - error
conf.itv.lower <- c()

## estimation + error
conf.itv.upper <- c()

## whether the confidence interval include mu or not. 
color <- c()

## count the number of confidence interval not include mu 
cnt <- 0

# n < 30 -> small sample 
for(i in 1:100){
  x <- rnorm(num.sample, mu, sigma)
  estimation[i] <- mean(x)
  t.star[i] <- qt(0.9, df = num.sample - 1)
  
  # bound of error = (t-critical value)*sample_sigma/sqrt(n)
  error[i] <- t.star[i] * sigma / sqrt(num.sample) 
  conf.itv.lower[i] <- estimation[i] - error[i] 
  conf.itv.upper[i] <- estimation[i] + error[i]
  
  # test if mu in the confidence interval
  if(conf.itv.lower[i] <= mu && conf.itv.upper[i] >= mu){
    color[i] <- "0"
  }else{
    color[i] <- "1"
    cnt <- cnt + 1
  }
}

## plot dataframe
point.plot <- data.frame(x = c(1:100), y = estimation)
line.plot <- data.frame(x = c(1:100), y = 50)
error.plot <- data.frame(x = c(1:100), ymin = conf.itv.lower, ymax = conf.itv.upper, col = color)

## plot 
ggplot() + 
  geom_point(data = point.plot, mapping = aes(x, y), shape = 1, stroke = 0.2) + 
  geom_line(data = line.plot, mapping = aes(x, y), linetype = "longdash", color = "red") + 
  geom_errorbar(data = error.plot, aes(x = x, ymin = ymin, ymax = ymax, color = col), linewidth = 0.2) + 
  scale_color_manual(values = c("black", "red")) + 
  theme(legend.position = "none") + 
  labs(title = "Understanding CL", 
       x = "Number of sample", y = "Confidence interval")

## Counting 
cnt



