x <- sample(x = 1:100, size = 100, replace = TRUE)
x
mean(x)
var(x)
sd(x)

y <- x
y[sample(x = 1:100, size = 20, replace = FALSE)] <- NaN
y
mean(y)
mean(y, na.rm = TRUE)

