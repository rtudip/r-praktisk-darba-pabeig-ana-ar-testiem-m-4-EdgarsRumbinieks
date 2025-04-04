#setwd("C:/Users/37126/Desktop/Pr_darbs")

kordat <- read.csv("variants9.txt", header = T, dec = ",", sep = "", strip.white = T)

kordat[,9:ncol(kordat)] <- lapply(kordat[,9:ncol(kordat)],as.factor)

sink("results.txt")

summary(kordat[,9:ncol(kordat)])

sl.by.b <- split(kordat, kordat$b)
print(sl.by.b)

kordat$Average <- apply(kordat[,c(1,2,6)],1,mean)

sapply(kordat[,c(1:8,12)], function(x)(tapply(x,kordat$f,sd)))

prockordat <- kordat[kordat$adj.r.squared>-0.7,]

funkc <- function(x) {1 - 1 / x}
prockordat$Slope <- funkc(prockordat$Slope)

print(prockordat)

sink()

svg("scatter.svg")
plot(kordat$MAD,kordat$Average, main = "Grafiks", xlab = "MAD", ylab = "Average")
dev.off()

svg("boxplot.svg")
boxplot(split(kordat$Intercept, kordat$f), main = "Kastes", xlab = "f faktors", ylab = "Intercept",
        col = c("blue","red",rep("blue",4)))
dev.off()