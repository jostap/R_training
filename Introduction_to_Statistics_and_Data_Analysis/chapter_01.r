#setwd("C:/Users/a-jostap/Documents/Statistical_Analysis/R/Introduction_to_Statistics_and_Data_Analysis")

#setwd("C:/Users/joser/Documents/R_Projects/Introduction_to_Statistics_and_Data_Analysis")

## From Apendix A
library(MASS)
painters
summary(painters)
summary(painters$School)
subset(painters, School=="F")
painters[painters[["School"]]=="F", c(1,2)]
subset(painters, Composition <= 6)
splitted = split(painters, painters$School)
splitted
lapply(splitted, summary)

## Exercises

# (b)
pizza <- read.csv('pizza_delivery.csv')
fix(pizza)
View(pizza)
pizza

# (c)
pizza2 <- pizza[1:5,1:5]
pizza2

write.csv(pizza2, file = 'pizza2.csv')
write.table(pizza2, file = 'pizza2.dat')
save(pizza2, file = 'pizza2.Rdata')

# (d)
pizza$NewTemperature <- 32 + 1.8 * pizza$temperature

# (e)
attach(pizza)
NewTemperature

# (f)
str(pizza)
dim(pizza)
colnames((pizza))
names(pizza)
nrow(pizza)

