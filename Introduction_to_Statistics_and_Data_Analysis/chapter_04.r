Class <- c(rep('1: Economy',62),rep('2: Business',25), rep('3: First',13))
Rating <- c(rep('1=poor',10),rep('2=fair',33),rep('3=good',15),rep('4=very good',4),
            rep('1=poor',0),rep('2=fair',3),rep('3=good',20),rep('4=very good',2),
            rep('1=poor',0),rep('2=fair',0),rep('3=good',5),rep('4=very good',8))

addmargins(table(Class,Rating))
addmargins(prop.table(table(Class,Rating)))

Vaccination <- c(rep('Vaccinated', 100), rep('Not vaccinated', 100))
Persons <- c(rep('Not affected', 90), rep('Affected', 10), rep('Not affected', 40), rep('Affected', 60))

addmargins(table(Vaccination, Persons))
addmargins(prop.table(table(Vaccination, Persons)))

library(lattice)
barchart(table(Class,Rating),horizontal=FALSE,stack=FALSE)
barchart(table(Class,Rating),horizontal=FALSE,stack=TRUE)

barchart(table(Vaccination, Persons),horizontal=FALSE,stack=FALSE)
barchart(table(Vaccination, Persons),horizontal=FALSE,stack=TRUE)

# Pearson's X^2 Statistic
chisq.test(table(Class,Rating))$expected
chisq.test(table(Class,Rating))$statistic

chisq.test(table(Vaccination, Persons))$expected
chisq.test(table(Vaccination, Persons))$statistic

# Cramer's V Statistic
library(vcd)
assocstats(xtabs(~Class+Rating))

# Contingency Coefficient C
library(vcd)
Cmax = sqrt((min(c(3,4))-1)/min(c(3,4)))
assocstats(xtabs(~Class+Rating))$cont/Cmax

# Association between ordinal and continuous variables
tweets <- c(25,11800,99,1934,199,2539,4334,952,3245,2468)
followers <- c(7194,43400000,324000,2330000,39000,189000,639000,688000,2690000,110000)
plot(tweets, followers)


# Correlation Coefficient
decathlon <- read.csv('decathlon.csv', row.names=1)
attach(decathlon)

cor(X.100m, X.Long.jump, method = 'pearson')
cor(X.100m, X.Long.jump, method = 'spearman')

cor(tweets, followers, method = 'pearson')
cor(tweets, followers, method = 'spearman')


# Measures Using Discordant and Concordant Pairs
library(ryouready)
ex <- matrix(c(7,11,26,10,15,31),ncol=3,byrow=T)
ord.gamma(ex)
ord.tau(ex)

ord.gamma(table(Vaccination, Persons))
ord.tau(table(Vaccination, Persons))

ord.gamma(table(Class,Rating))
ord.tau(table(Class,Rating))

ord.gamma(table(tweets, followers))
ord.tau(table(tweets, followers))

#Visualization of Variables from Different Scales
boxplot(time~branch)
plot.ecdf(time[branch=='East'])
plot.ecdf(time[branch=='West'], add=TRUE)
plot.ecdf(time[branch=='Centre'], add=TRUE)
