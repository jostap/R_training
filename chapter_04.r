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

################
# Exercise 4.1 #
################

cafeX <- c(3,8,7,9,5)
cafeY <- c(6,7,10,8,4)
cor(cafeX, cafeY, method = 'spearman')

################
# Exercise 4.2 #
################

CarType <- c(rep('Car', 58), rep('Car(diesel)', 60), rep('Motorbike', 32))
Satisfaction <- c(rep('Satisfied', 33), rep('Unsatisfied', 25), rep('Satisfied', 29), rep('Unsatisfied', 31),
                  rep('Satisfied', 12), rep('Unsatisfied', 20))

addmargins(table(CarType, Satisfaction))
addmargins(prop.table(table(CarType, Satisfaction)))

# Pearson's X^2 Statistic
chisq.test(table(CarType, Satisfaction))$expected
chisq.test(table(CarType, Satisfaction))$statistic

# Cramer's V Statistic
library(vcd)
assocstats(xtabs(~CarType+Satisfaction))

# Contingency Coefficient C
library(vcd)
Cmax = sqrt((min(c(2,3))-1)/min(c(2,3)))
assocstats(xtabs(~CarType+Satisfaction))$cont/Cmax

################
# Exercise 4.3 #
################

speedLimits <- c(55,55,60,60,75)
trafficDeaths <- c(4.1,4.7,4.3,5.1,6.1)
plot(speedLimits, trafficDeaths)

cor(speedLimits, trafficDeaths, method = 'pearson')
cor(speedLimits, trafficDeaths, method = 'spearman')

speedLimits1 <- c(55,55,60,60,75,70)
trafficDeaths1 <- c(4.1,4.7,4.3,5.1,6.1,3.1)
plot(speedLimits1, trafficDeaths1)

cor(speedLimits1, trafficDeaths1, method = 'pearson')
cor(speedLimits1, trafficDeaths1, method = 'spearman')

################
# Exercise 4.4 #
################

RescueStatus <- c(rep('Rescued', 1510), rep('No Rescued', 718))
TravelClass <- c(rep('1 Class', 135), rep('2 Class', 160), rep('3 Class', 541), rep('Staff', 674),
                  rep('1 Class', 202), rep('2 Class', 125), rep('3 Class', 180), rep('Staff', 211))

addmargins(table(RescueStatus, TravelClass))
addmargins(prop.table(table(RescueStatus, TravelClass)))

# Pearson's X^2 Statistic
chisq.test(table(RescueStatus, TravelClass))$expected
chisq.test(table(RescueStatus, TravelClass))$statistic

# Cramer's V Statistic
library(vcd)
assocstats(xtabs(~RescueStatus+TravelClass))

# Contingency Coefficient C
library(vcd)
Cmax = sqrt((min(c(2,4))-1)/min(c(2,4)))
assocstats(xtabs(~RescueStatus+TravelClass))$cont/Cmax

################
# Exercise 4.5 #
################

# c)

temp <- c(-6,-5,2,4,7,15,17,19,13,9,4,0,10,10,14,17,22,24,26,27,22,19,14,12,1,0,5,9,14,20,23,24,21,14,9,4)
ho <- c(91,89,76,52,42,36,37,39,26,27,68,92,13,21,42,64,79,81,86,92,36,23,13,41,23,82,40,45,39,43,50,95,64,78,9,12)
Z <- c(rep('Davos',12),rep('Polenca',12),rep('Basel',12))
plot(temp, ho)

cor(temp, ho, method = 'pearson')
cor(temp, ho, method = 'spearman')

cor(temp[Z=='Davos'], ho[Z=='Davos'])
cor(temp[Z=='Basel'], ho[Z=='Basel'])
cor(temp[Z=='Polenca'], ho[Z=='Polenca'])

#################
# Exercise 4.8  #
#################

# a)
decathlon <- read.csv("decathlon.csv", row.names=1)
attach(decathlon)
cor(X.Discus,X.High.jump)
# c)
cor(decathlon)
# d)
cor(na.omit(decathlon))

# and....
library(corrplot) # nice summary of results with package corrplot(not mentioned in solutions)
corrplot(cor(na.omit(decathlon)),method="number",col="black",cl.pos="n",tl.col="black")

detach(decathlon)

################
# Exercise 4.9 #
################

pizza <- read.csv("pizza_delivery.csv")
pizza$tempcat <- cut(pizza$temperature, breaks=c(0,65,100))
pizza$timecat <- cut(pizza$time, breaks=c(0,30,100))
attach(pizza)

# a)
addmargins(table(tempcat,timecat))

# b) 
(101*261)/(213*691)

# c) 
library(vcd)          # download if necessary (install.packages("vcd"))
library(ryouready)    # download if necessary (install.packages("ryouready"))
library(lattice)      
assocstats(xtabs(~tempcat+timecat))
ord.gamma(table(tempcat,timecat))
ord.tau(table(tempcat,timecat))

# simple
barchart(table(tempcat,timecat),horizontal=F,stack=T)
# more options 
barchart(table(tempcat,timecat),horizontal=F,stack=T,auto.key=list(space="top", columns=2,points=FALSE,cex=1.5,rectangles=TRUE,title="timecat"),col=gray.colors(2),ylab=list(label="absolute frequencies",  cex=1.75), xlab=list(cex=1.75),par.names.text=list(cex=2),par.settings=list(superpose.polygon=list(col=gray.colors(2))))

# d) 
# simple
plot(time,temperature)

# nicer, also saved as pdf
pdf(file="exercise_4.9_d.pdf") 
par(mar= c(5, 5, 2, 2))
plot(time, temperature, pch=19, cex.axis=1.75,cex.lab=1.75,cex=1.5,xlab="Time",ylab="Temperature")
dev.off()
cor(time,temperature)
cor(time,temperature,method="spearman")

# e)
# simple
boxplot(temperature~driver)
boxplot(temperature~operator)

# nicer, also saved as pdf 
pdf(file="exercise_4.9_e1.pdf") 
par(mar= c(5, 5, 2, 2))
boxplot(temperature~driver,xlab="Driver",ylab="Temperature", cex.axis=1.25,lwd=3,cex.lab=1.75,cex.main=1.75)
dev.off()

pdf(file="exercise_4.9_e2.pdf") 
par(mar= c(5, 5, 2, 2))
boxplot(temperature~operator,xlab="Operator",ylab="Temperature", cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75)
dev.off()

cor(temperature,pizzas)
cor(temperature,bill)

