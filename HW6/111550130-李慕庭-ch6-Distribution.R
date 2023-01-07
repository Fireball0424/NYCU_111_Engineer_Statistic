#111550130 Mu-Ting Lee ch6 distribution

### Library
library(ggplot2)
library(MASS)
library(moments)

### import data
df <- data.frame(x = c(37.0, 37.5, 38.1, 40.0, 40.2,
                   40.8, 41.0, 42.0, 43.1, 43.9,
                   44.1, 44.6, 45.0, 46.1, 47.0, 
                   50.2, 55.0, 56.0, 57.0, 58.0,
                   62.0, 64.3, 68.8, 70.2, 74.5))
density <- density(df$x)
df_dens <- data.frame(x = density$x, y = density$y)
density$bw #5.263079

vir <- seq(20, 80, 0.1)

### Weibull 
Weibull <- fitdistr(df$x, "weibull")
Weibull_alpha = Weibull$estimate[1] 
Weibull_beta = Weibull$estimate[2]
d_weibull = dweibull(vir, shape = Weibull_alpha, scale = Weibull_beta)

### Normal
Normal <-fitdistr(df$x, "normal")
Normal_mean = Normal$estimate[1]
Normal_sd = Normal$estimate[2]
d_normal = dnorm(vir, mean = Normal_mean, sd = Normal_sd)

### Draw the fit plot
fit = data.frame(x = vir, wei = d_weibull, nor = d_normal)
ggplot() + 
  scale_x_continuous(breaks = seq(25, 100, 25)) + 
  geom_line(data = df_dens, aes(x, y), color = "black") + 
  geom_line(data = fit, aes(x, wei), color = "red") + 
  geom_line(data = fit, aes(x, nor), color = "blue", linetype = 2) + 
  labs(x = 'Material Strength', y = 'Density', 
       title = 'Fitting Distribution')

### Draw the qq plot
ggplot() + 
  aes(sample = df$x) + 
  stat_qq(distribution = qnorm) + 
  stat_qq_line(line.p = c(0.25, 0.75), col = "blue")+
  labs(x = "Theoretical Normal Distribution Quantile",
       y = "Observed Data", title = "QQplot")

## Skewness and Kurtosis
sk <- skewness(df$x)
kt <- kurtosis(df$x)

sk #0.7952912
kt #2.413146
