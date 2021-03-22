Class <- c(rep('1: Economy',62),rep('2: Business',25), rep('3: First',13))
Rating <- c(rep('1=poor',10),rep('2=fair',33),rep('3=good',15),rep('4=very good',4),
            rep('1=poor',0),rep('2=fair',3),rep('3=good',20),rep('4=very good',2),
            rep('1=poor',0),rep('2=fair',0),rep('3=good',5),rep('4=very good',8))

addmargins(table(Class,Rating))
addmargins(prop.table(table(Class,Rating)))

library(lattice)
barchart(table(Class,Rating),horizontal=FALSE,stack=FALSE)
barchart(table(Class,Rating),horizontal=FALSE,stack=TRUE)

# Pearson's X^2 Statistic
chisq.test(table(Class,Rating))$expected
chisq.test(table(Class,Rating))$statistic

# Cramer's V Statistic
library(vcd)
assocstats(xtabs(~Class+Rating))

# Contingency Coefficient C
library(vcd)
Cmax = sqrt((min(c(3,4))-1)/min(c(3,4)))
assocstats(xtabs(~Class+Rating))$cont/Cmax