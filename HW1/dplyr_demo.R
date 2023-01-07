#install.packages("dplyr")
library(dplyr)

#install.packages("gapminder")
library(gapminder)

df1 <- gapminder %>%
  select(country, lifeExp) %>%
  filter(country == "South Africa" |
           country == "Ireland")

#install.packages("ggplot2")  
library(ggplot2)


gapminder  %>%
  filter(gdpPercap < 50000)  %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp, color=continent,size=pop))+
  geom_point(alpha=0.3)+
  geom_smooth(method = lm)


