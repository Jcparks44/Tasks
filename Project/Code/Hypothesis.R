#A variety of pathogenic bacteria that develop single mutations to resist increasing concentrations of multiple antibiotics will have a lower relative doubling number in set unit time, as a result of fitness cost. 
setwd('~/Desktop/Evolution/Tasks/Project/Data')
Data <- read.csv('~/Desktop/Evolution/Tasks/Project/Data/Final_Project_Updated.csv', 
	stringsAsFactors=F)

colnames(Data)
head(Data)
Fitness <- Data[,2]
Resistance <- Data[,1]

# all data
Resistance1 <- Resistance
Fitness1 <- Fitness

# just mutants
#Resistance1 <- Resistance1[which(Resistance1 > 1.0)]
#Fitness1 <- Fitness1[which(Resistance1 > 1.0)]
pdf('Graph1_FinalProject.pdf', height=5, width=5)
Cols <- c("#e41a1c", "#377eb8", "#4daf4a")
par(mar=c(4,5,1,1), mgp=c(2.5, 0.25, 0), las=1, tck=-0.01)
plot(Resistance1, Fitness1, xlab="Fold-Increase MIC", ylab="Relative fitness", main='Relative Fitness as a Function of Resistance', log='x', ylim=c(0, 1.5), pch=21, bg='gray70')

# locally smoothed regression
x2 <- loess(Fitness1~Resistance1, span=1)
px2 <- predict(x2)
lines(Resistance1[order(Resistance1)], px2[order(Resistance1)], col=Cols[1], lty=2, lwd=2)

# polynomial regression
Cubic_x <- lm(Fitness1 ~ poly(Resistance1,2))
pCubic <- predict(Cubic_x)
lines(Resistance1[order(Resistance1)], pCubic[order(Resistance1)], col=Cols[2], lwd=2)
dev.off()
# linear regression
x <- lm(Fitness1~Resistance1)
abline(x, col=Cols[3], lty=3, lwd=2)

Fitness2 <- Data[,9]
Fitness2
Resistance2 <- Data[,8]
Resistance2
pdf('Graph2_Project.pdf', height=5 , width=5)
plot(Resistance2, Fitness2, xlab='log MIC(mg/L)', ylab='Relative Fitness', main='Relative Fitness as a Result of Resistance') 
abline(lm(Fitness2 ~ Resistance2), col='red')
dev.off()

Fitness3 <- Data[,16]
Fitness3
Resistance3 <- Data[,15]
Resistance3
pdf('Graph3_Project.pdf', height=5 , width=5)
plot(Resistance3, Fitness3, xlab='MIC ciproflaxin(mg/L)', ylab='Relative Fitness of E. Coli', main='Relative Fitness as a Result of Resistance') 
abline(lm(Fitness3 ~ Resistance3), col='red')
dev.off()


Fitness4 <- Data[,24]
Fitness4
Resistance4 <- Data[,23]
Resistance4
pdf('Graph4_Project.pdf', height=5 , width=7)
plot(Resistance4, Fitness4, xlab='MIC(mg/L)', ylab='Relative Fitness', main='Relative Fitness as a Result of Secondary Resistance Mutations')
abline(lm(Fitness4 ~ Resistance4), col='red')
dev.off()

Fitness5 <- Data[,31]
Fitness5
Resistance5 <- Data[,30]
Resistance5
pdf('Graph5_Project.pdf', height=5 , width=5)
plot(Resistance5, Fitness5, xlab='MIC(mg/L)', ylab='Relative Fitness', main='Relative Fitness as a Result of Resistance')
abline(lm(Fitness5 ~ Resistance5), col='red')
dev.off()

Fitness6 <- Data[,40]
Fitness6
Resistance6 <- Data[,39]
Resistance6
pdf('Graph6_Project.pdf', height=5 , width=5)
plot(Resistance6, Fitness6,  xlab='MIC(µg/mL)', ylab='Relative Fitness', main='Relative Fitness as a Result of Resistance')
abline(lm(Fitness6 ~ Resistance6), col='red')
dev.off()
