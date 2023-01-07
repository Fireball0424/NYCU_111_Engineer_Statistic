# File: R_Data_visualization_b.R
# Course: Ch4.R.Data.Visualization
# Section: load dataset CO2 in R 
#         coord_cartesian,
#         geom_point, geom_smooth,
#         facet_wrap, labs,
#         theme_bw, theme
# Author: vvn weian chao, CoLLab cc, vvnchao@gmail.com
# Date: 2022-10-17

data()
?CO2
View(CO2)
# Carbon Dioxide Uptake in Grass Plants
names(CO2)
# Plant- an ordered factor, Qn1 < Qn2 < Qn3...
# Type- Quebec, Mississippi
# Treament- nonchilled, chilled
# conc- ambient carbon dioxide concentrations [mL/L]
# uptake- uptake rates (umol/m^2 sec)

# load library
library(ggplot2)
ggplot(data = CO2, 
       aes(conc, uptake, color = Treatment))+
  coord_cartesian(ylim = c(0,50))+
  geom_point(size = 2, shape = 15, alpha = 0.5)+
  geom_smooth()+
  facet_wrap(CO2$Type)+
  labs(title = "Understanding uptake for grass plant"
       , x = "ambient CO2 concentration [mL/L]"
       , y = "uptake [umol/m^2 sec]")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45))
  
