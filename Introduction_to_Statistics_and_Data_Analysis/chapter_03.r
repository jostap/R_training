weather <- c(22, 24, 21, 22, 25, 26, 25, 24, 23, 25, 25, 26, 27, 25, 26,
             25, 26, 27, 27, 28, 29, 29, 29, 28, 30, 29, 30, 31, 30, 28, 29)
weather
mean(weather)
summary(weather)


weighted.mean(c(22.5,27.5,32.5),c(12/31,18/31,1/31))
median(weather)
quantile(weather)
quantile(weather, probs = c(0, 0.25, 0.5, 0.75,1))
?qqplot

y <- pizza[pizza[,6] == 'Domenico',] 
y$time

x <- pizza[pizza[,6] == 'Luigi',] 
x$time
par(mar= c(5, 5, 2, 2))
qqplot(x$time, y$time, xlim=c(10,60),ylim=c(10,60), xlab = 'Delivery time from Luigi', ylab = 'Delivery time from Domenico')
lines(c(10,60),c(10,60),type="l",col="blue",lwd=2)

z <- pizza[pizza[,6] == 'Mario',] 
z$time

w <- pizza[pizza[,6] == 'Salvatore',] 
w$time

par(mar= c(5, 5, 2, 2))
qqplot(z$time, w$time, xlim=c(10,60),ylim=c(10,60), xlab = 'Delivery time from Mario', ylab = 'Delivery time from Salvatore')
lines(c(10,60),c(10,60),type="l",col="blue",lwd=2)

# Measures of Dispersion
var(time)
air <- c(30,25,12,45,50,52,38,39,45,33)
mean(air)
median(air)
var(air)
sd(air)
scale(air)

# Box plot
boxplot(time)
boxplot(time, range=1.5)
boxplot(temperature, range=1.5)
boxplot(weather, range=1.5)

# Lorenz Curve
library(ineq)
x <- c(20, 20, 20, 20, 20)
plot(Lc(x))

plot(Lc(time))

################
# Exercise 3.1 #
################

distance <- c(12.5,29.9,14.8,18.7,7.6,16.2,16.5,27.4,12.1,17.5)
altitude <- c(342,1245,502,555,398,670,796,912,238,466)

# a) 
mean(distance)
mean(altitude)

median(distance)
median(altitude)

# b)
quantile(distance,probs=0.75)
quantile(distance,probs=0.25)
quantile(altitude,probs=0.75)
quantile(altitude,probs=0.25)

# c)
quantile(distance,probs=0.75)-quantile(distance,probs=0.25)
quantile(altitude,probs=0.75)-quantile(altitude,probs=0.25)

amd <- function(mv){1/length(mv)*sum(abs(mv-median(mv)))}
amd(distance)
amd(altitude)

var(altitude)
var(distance)

# e)

pdf(file="exercise_3.1_e1.pdf")
par(mar= c(5, 5, 2, 2))
boxplot(altitude,xlab="",ylab="Altitude (in m)", cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75) 
dev.off()

pdf(file="exercise_3.1_e2.pdf")
par(mar= c(5, 5, 2, 2))
boxplot(distance,xlab="",ylab="Distance (in km)", cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75) 
dev.off()

# f)
weighted.mean(c(10,17.5,25),c(4/10,4/10,2/10))

