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
sort(distance)
sort(altitude)
quantile(distance,probs=0.75)
quantile(distance,probs=0.25)
quantile(altitude,probs=0.75)
quantile(altitude,probs=0.25)

# c)
Dd <- quantile(distance,probs=0.75) - quantile(distance,probs=0.25)
Da <- quantile(altitude,probs=0.75) - quantile(altitude,probs=0.25)

Dd
Da

# Absolute Median Deviation
amd <- function(mv){
  (1/length(mv))*sum(abs(mv-median(mv)))
}
amd(distance)
amd(altitude)

myvar <- function(mv) {
  (1/length(mv))*sum((mv-mean(mv))^2)
}

myvar(distance)
myvar(altitude)

mysd <- function(mv) {
  sqrt(myvar(mv))
}

mysd(distance)
mysd(altitude)

var(distance)
var(altitude)
sd(distance)
sd(altitude)


# e)

par(mar= c(5, 5, 2, 2))
boxplot(altitude,xlab="",ylab="Altitude (in m)", cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75) 
dev.off()

par(mar= c(5, 5, 2, 2))
boxplot(distance,xlab="",ylab="Distance (in km)", cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75) 
dev.off()

# f)
weighted.mean(c(10,17.5,25),c(4/10,4/10,2/10))

#################
# Exercise 3.2  #
#################

# a)
casino <- c(200, 600, -200, -200, -200, -100, -100, -400, 0, -500)

# b)
# Create the function getmode.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(casino)
rcasino <- quantile(casino,probs=0.75) - quantile(casino,probs=0.25)
rcasino

#################
# Exercise 3.6  #
#################

mymode <- function(vec){
  mt <- table(vec)
  names(mt)[mt == max(mt)]
}

mymode(c(1,2,3,3,4))
mymode(c("1","1","2"))

################
# Exercise 3.9 #
################

# d)
u2 <- c(0,0.25,0.5,0.75,1)
v2 <- c(0,0.044,0.168,0.428,1)

library(ineq)
investment <- c(800,10300,4700,2200)
plot(Lc(investment))
ineq(investment)

# d) "by hand", more complicated solution than "plot(Lc(investment))". 
# We plot u against v, and make the graph nice using the options. 

par(mar= c(5, 5, 2, 2))
plot(u2,v2,type="l",xlab=expression(u[i]),ylab=expression(v[i]), cex.axis=1.75,cex.lab=1.75,cex=1.75,cex.main=1.5,lwd=3, xaxt = "n",bty="n", yaxt="n",las=1)
axis(1, at=round(u,digits=2), labels=c("0","0.25","0.5","0.75","1"), cex.axis=1.75,cex.lab=1.75,cex=1.75,cex.main=1.5,lwd=3)
axis(2, at=round(v,digits=2), labels=c("0","0.044","0.168","0.428","1"), cex.axis=1.75,cex.lab=1.75,cex=1.75,cex.main=1.5,lwd=3,las=1)
dev.off()


#################
# Exercise 3.10 #
#################

#a)
pizza <- read.csv("pizza_delivery.csv")
attach(pizza)
summary(pizza[,c("time","temperature","bill","pizzas")])

# b)
quantile(time,probs=0.99)
quantile(temperature,probs=0.99)

# c)
amdev <- function(mv){1/length(mv)*sum(abs(mv-mean(mv)))}
amdev(temperature)

# d)
sc.time <- scale(time)
mean(sc.time)
var(sc.time) 

# e)
# Simple solution
boxplot(temperature, range = 0)
boxplot(time, range = 0)

# nicer graph, using the options

par(mar= c(5, 5, 2, 2))
boxplot(temperature,range=0,xlab="",ylab="Temperature", ylim=c(30,100), cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75)
dev.off() 

par(mar= c(5, 5, 2, 2))
boxplot(time,range=0,xlab="",ylab="Delivery Time (in minutes)", ylim=c(10,60), cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75)
dev.off() 

# f)
?cut
tc <- cut(time,breaks=seq(10,60,10))
tc
weighted.mean(c(15,25,35,45,55),table(tc)/sum(table(tc)))
mean(time)

# g)
# fast and simple
qqplot(time[driver=="Luigi"],time[driver=="Domenico"])
qqplot(time[driver=="Mario"],time[driver=="Salvatore"])

# long and nice
par(mar= c(5, 5, 2, 2))
qqplot(time[driver=="Luigi"],time[driver=="Domenico"],xlim=c(10,60),ylim=c(10,60),xlab="Delivery time from Luigi",ylab="Delivery time from Domenico", cex.axis=1.75, ,cex.lab=1.75,cex.main=1.75, cex=1.75)
abline(a=0,b=1,lwd=3)
dev.off()

par(mar= c(5, 5, 2, 2))
qqplot(time[driver=="Mario"],time[driver=="Salvatore"],xlim=c(10,60),ylim=c(10,60),xlab="Delivery time from Mario",ylab="Delivery time from Salvatore", cex.axis=1.75, ,cex.lab=1.75,cex.main=1.75, cex=1.75)
abline(a=0,b=1,lwd=3)
dev.off()