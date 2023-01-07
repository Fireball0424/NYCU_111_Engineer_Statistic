# 111550130 Mu-Ting Lee ch8 Probability

## import library
library(ggplot2)

## Setting constant value
n.typhoon = 20

## (1) Calculate the theoretical value
ans <- 1
for(i in 1:n.typhoon)
  ans <- ans * (1 - 0.1)
print(ans)

## (2) Stimulus n = 8
set.seed(1)
n <- 8
m <- 0
for(i in 1:n)
  n.strike <- 0
  for(j in 1:n.typhoon){
    cur <- sample(0:99, 1)
    if(cur < 10){
      n.strike <- n.strike + 1
    }
  }
  if(n.strike == 0) m <- m + 1
prob <- m / n
print(prob)

## stimulus n = 2^3~2^15
n <- 8
x <- c()
y <- c()
for(power in 3:15){
  m <- 0
  for(i in 1:n){
    cnt <- 0
    for(j in 1:n.typhoon){
      cur <- sample(0:99, 1)
      if(cur < 10) cnt <- cnt + 1
    }
    if(cnt == 0) m <- m + 1
  }
  x[power - 2] <- n
  y[power - 2] <- m/n
  n <- n * 2
}
x
y
## graph 
df <- data.frame(x = x, y = y)
data.line <- data.frame(x = x, y = ans)
ggplot() + 
  geom_point(df, mapping = aes(x, y))  + 
  geom_line(data.line, mapping = aes(x, y), linetype = "longdash")+
  labs(x = "Number of Random Sampling", 
       y = "Probability of n.strike = 0", 
       title = "Probability of Typhoon Strike Taiwan")

