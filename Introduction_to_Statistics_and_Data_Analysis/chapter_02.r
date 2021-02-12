table(branch)
table(branch)/length(branch)

sv <- c(rep(1,4), rep(2,16), rep(3,90), rep(4,70), rep(5,20))
plot.ecdf(sv)

barplot(table(branch))
barplot(table(branch)/length(branch))

pie(table(sv))

# EDCF for continuous variables
plot.ecdf(time)

pizza$DeliveryTime <- ifelse(pizza$time <= 10, '[0; 10]', 
                             ifelse(pizza$time <= 15, '(10; 15]',
                             ifelse(pizza$time <= 40, '(15; 40]', '.')))


plot.ecdf(DeliveryTime)