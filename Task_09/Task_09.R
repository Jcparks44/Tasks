setwd('~/Desktop/Evolution/Tasks/Task_09')
library('phytools')

##QUESTIONS 1-3####

trees <- list()
births <- c()
Fractions <- c()

for(i in 1:100) {
	births[i] <- runif(1)
	Fractions[i] <- runif(1)
	trees[[i]] <- pbtree(b = births[i], d = (births[i] * Fractions[i]), n = 100, nsim = 1)
}
trees
trees[[i]]
plot(trees[[i]])

#?pbtree
library('geiger')
#?sapply

##  QUESTION 4###
#install.packages('TreeTools')
library('TreeTools')
#Y


tips <- sapply(trees, NTip)
logtips <- log(tips)
diversification <- sapply(trees, bd.ms)
plot(diversification, logtips, xlab='net diversification', ylab='log of total number of tips')
abline(lm(diversification~logtips), col='red')
#My graph shows a strong positive correlation between diversification and number of tips, which is waht I would expect to see.  
cor(diversification, logtips)
#positive correlation according to R. cor=0.28

### QUESTION 5 #####
speciation <- sapply(trees, bd.km)

#for (t in 1:length(trees))	{

i <- 1

numtips <- c()
avgBL <- c()

for ( i in 1:length(trees))	{
	# choose tree
	y <- trees[[i]]
	# find number of tips
	numtips[i] <- Ntip(y)
	# find average branch length
	avgBL[i] <- mean(y$edge.length)
}
plot(speciation, avgBL, xlab='speciation rate', ylab='average branch length')
#My graph shows that branch length is inversely proportional to speciation rate. 

### Question 6 ####
cor(speciation, avgBL)
#correlation = -0.26

### Question 7 ####
which.max(tips)
bigTree <- trees[[66]]
plot(bigTree)

rates <- c()
traits <- list()
for (i in 1:100) {
	rates[i] <- runif(1)
	traits[[i]] <- fastBM(tree = bigTree, sig2 = rates[i])
}

### Question 8 ###
avgtrait <- sapply(traits, mean)
avgtrait
avgrate <- sapply(rates, mean)
avgrate
correlation <- cor(avgtrait, avgrate)
print(correlation)
plot(avgrate, avgtrait)
abline(lm(avgrate~avgtrait), col='purple')
#my simulation got a 0.09 correlation. This would make sense because as rate of newly formed tips increases, more average traits would be present. 

### Question 9 ###
vartraits <- sapply(traits, var)
cor(vartraits, rates)
#There is a positive correlation between variance of traits and rates, which would make sense because with the increase of rates, we would see a higher variation between traits. 
### Question 10 ####
trait1 <- traits[1]
trait1
trait2 <- traits[2]
trait2
traitmat <- cbind(traits[[1]], traits[[2]])
traitmat
var(traitmat)
cor(traitmat[,1], traitmat[,2])
#The correlation is near zero, so I wouldn't consider it signifcant. 
plot(traitmat[,1], traitmat[,2])
abline(lm(traitmat[,1]~traitmat[,2]), col='pink')




