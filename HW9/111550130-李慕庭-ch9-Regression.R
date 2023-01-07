#111550130 Mu-Ting Lee ch9 Regression 

## Import the library
library(ggplot2)

## Import the data
data("cars")
df <- data.frame(x = cars$speed, y = cars$dist)
n <- length(df$x)

## Fitting straight line
sl <- lm(y ~ x, data = df)
a1 = sl$coefficients[1]
b1 = sl$coefficients[2]
a1 <- round(a1, 2)
b1 <- round(b1, 2)
a1
b1


## calculate r and sse of straight line
r.sl <- cor(df$x, df$y, method="pearson")
r <- round(r.sl, 2)
r
r2.sl<- round(r.sl ^ 2, 2)
r2.sl
anova.sl <- anova(sl)
sse.sl <- anova.sl$`Sum Sq`[2]
se.sl <- round(sqrt(sse.sl / (n - 2)), 2)
se.sl

## Fitting power line
lgx <- log(df$x)
lgy <- log(df$y)
pl <- lm(lgy ~ lgx)
e <- exp(1)
a2 <- e ^ pl$coefficients[1]
b2 <- pl$coefficients[2]
a2 <- round(a2, 2)
b2 <- round(b2, 2)
a2
b2

## Calculate the r and sse of curve 
sse.pl <- 0.
ssto.pl <- 0.
ymean <- mean(df$y)
for(i in 1:n){
  ypre <- a2*df$x[i]^b2
  sse.pl <- sse.pl + (df$y[i] - ypre) ^ 2
  ssto.pl <- ssto.pl + (df$y[i] - ymean) ^ 2
}

r2.pl <- round( 1 - sse.pl / ssto.pl, 2)
r2.pl
se.pl <- round(sqrt(sse.pl / (n - 2)), 2)
se.pl

## curve  
prex <- seq(0, 30, 0.01)
prey <- a2*prex ^ b2
curve <- data.frame(prex, prey)

## Plot
sub.text1 <- expression(paste("r"^"2", ":0.65 se:15.38(line) ", "r"^"2", ":0.66 se:15.22(curve)"))
sub.text2 <- expression(paste("dist=-17.58+3.93*speed(dashline), dist=0.48speed"^"1.6"))
ggplot() + 
  geom_point(data = df, mapping = aes(x, y)) + 
  geom_abline(slope = b1, intercept = a1, linetype = 2) + 
  geom_line(curve, mapping = aes(prex, prey)) + 
  labs(title = "Scatter Plot : Speed v.s. Distance",
       subtitle = sub.text1,
       caption = sub.text2,
       x = "Speed, mph", 
       y = "Distance, ft")
  

