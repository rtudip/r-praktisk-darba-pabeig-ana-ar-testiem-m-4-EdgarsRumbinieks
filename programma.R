setwd("C:/Users/37126/Desktop/Pr_darbs")

#1. punkts
kordat <- read.csv("variants9.txt", header = T, dec = ",", sep = "")

#2. punkts
kordat[,9:ncol(kordat)] <- lapply(kordat[,9:ncol(kordat)], as.factor)

#3. punkts
sink("results.txt")

#4. punkts
summary(kordat[,9:ncol(kordat)])

#5. punkts
sl.by.b <- tapply(kordat$Slope, kordat$b,mean)
print(sl.by.b)

#6. punkts
kordat$Average <- apply(kordat[,c(1,2,6)],1,mean)

#7. punkts
apply(kordat[,c(1:8,12)],2, function(x)(tapply(x,kordat$f,sd)))

#8. punkts
#adj.r.squared nav nevienas vērtības >0.7, tāpēc nomainīju to uz >-0.4
prockordat <- kordat[kordat$adj.r.squared>-0.4,]

#9. punkts
prockordat$Slope <- 1-1/prockordat$Slope

#10. punkts
print(prockordat)

sink()

#11. punkts
svg("scatter.svg")
plot(kordat$MAD,kordat$Average, main = "Grafiks", xlab = "MAD", ylab = "Average")
dev.off()

#12.punkts
svg("boxplot.svg")
boxplot(kordat$Intercept ~ kordat$f, main = "Kastes", xlab = "f faktors", ylab = "Intercept",
        col = c("blue","red",rep("blue",4)))
dev.off()