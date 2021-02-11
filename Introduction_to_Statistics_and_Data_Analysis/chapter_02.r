table(branch)
table(branch)/length(branch)

sv <- c(rep(1,4), rep(2,16), rep(3,90), rep(4,70), rep(5,20))
plot.ecdf(sv)

barplot(table(branch))
barplot(table(branch)/length(branch))

pie(table(sv))