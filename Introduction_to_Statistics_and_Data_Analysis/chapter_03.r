weather <- c(22, 24, 21, 22, 25, 26, 25, 24, 23, 25, 25, 26, 27, 25, 26,
             25, 26, 27, 27, 28, 29, 29, 29, 28, 30, 29, 30, 31, 30, 28, 29)
weather
mean(weather)


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
sd(air)
scale(air)

# Box plot
boxplot(time, range=1.5)
boxplot(temperature, range=1.5)
boxplot(weather, range=1.5)


