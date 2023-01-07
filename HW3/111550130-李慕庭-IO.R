#111550130 §õ¼}®x IO

#import rio package
#install.packages("rio", lib = "C://Program Files//R//R-4.2.1//library")
library("rio")

#import data
df <- import(file = "HW3//weatherdata.xlsx")

#sort data in rain
df_sorted_rain <- df[order(df$Rain, decreasing = TRUE), ]
head(df_sorted_rain)

max_rain = df_sorted_rain$Rain[1]
df_max_rain = df[df$Rain == max_rain, ]
max_rain_date = df_sorted_rain$Date[1]

#output text
cat(sprintf("The data number : %d", nrow(df_max_rain)), "\n", file = "Output.txt")
cat(sprintf("The maximum rainfall per 5 min.: %.1f mm", max_rain), "\n", file = "Output.txt", append = TRUE)
cat(sprintf("The date and time : %s", as.character(max_rain_date)), "\n", file = "Output.txt", append = TRUE)

