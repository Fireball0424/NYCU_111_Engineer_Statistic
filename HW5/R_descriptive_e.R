# File: R_descriptive_e.R
# Course: Ch5.R.Descriptive_Statistics
# Section: (0)read datasets from R
#          (1)boxplot
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-10-24

# (0) IRIS dataset
?iris
names(iris)
str(iris)

# (1) boxplot
?geom_boxplot
ggplot(data = iris, aes(Sepal.Length,Species))+
  geom_point(data = iris, aes(Sepal.Length),
             size = 1.0)+
  geom_boxplot(outlier.color = "red",
               outlier.shape = 10,
               outlier.size = 3,
               coef = 1.0)+
  #coord_flip()+
  labs(title = "IRIS Dataset for Boxplot",
       y = "Species", x = "Sepal Length")+
  theme_bw()
