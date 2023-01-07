#111550130 Mu-Ting Lee ch11 testing 

## import library and data
data(iris)
df <- iris

library(ggplot2)
library(dplyr)
library(BSDA)

## Normality test
setosa.index <- which(df$Species == "setosa")
versicolor.index <- which(df$Species == "versicolor")
setosa <- df$Sepal.Length[setosa.index]
versicolor <- df$Sepal.Length[versicolor.index]

shapiro.test(setosa)
shapiro.test(versicolor)

## boxplot
df %>% 
  filter(Species == "versicolor" | Species == "setosa") %>% 
  ggplot(mapping = aes(Species, Sepal.Length)) + 
    geom_boxplot() + 
    coord_flip() + 
    geom_point() + 
    labs(title = "Boxplot Sepal Data", 
         x = "Species", y = "Sepal Length(mm)")

## test statistic
s1 <- sd(versicolor)
s2 <- sd(setosa)
var.test(versicolor, setosa, ratio = 1)
z.test(x = versicolor, y = setosa, 
       alternative = "two.sided", 
       sigma.x = s1, sigma.y = s2,
       mu = 0, conf.level = 0.95)
