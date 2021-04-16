setwd('~/Desktop/Evolution/Tasks/Task_10')
library('diversitree')
transition_0to1 <- 0.1
transition_1to0 <- 0.1

speciation_0 <- 0.2
extinction_0 <- 0.15

speciation_1 <- 0.4
extinction_1 <- 0.1	

maxN <- 1e3
maxT <- 50

Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)

simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
str(simTree)
#?tree.bisse()

stateTable <- table(simTree$tip.state)
#relative frequency of state 0 and state 1:
stateTable / sum(stateTable)

###THESE ARE MY NOTES FOR REFERENCE####
#Try different parameters, calculate the frequency for each, and make plot/plots that show how different rates influence the frequency of a trait. 

#Transition rate: shift from one trait to another
#transition_0to1 etc. 

## net diversification is lambda - mu ######

# 1. is freq of state 1 ever higher than state 0 when the diversification of state 1 is lower than that of state 0?


#The only way I could get state 1 frequency higher when its net diversification was lower was if it was within 0.01 below state 0. My plot shows that the least possible difference between net diversification rates allows state 1 to be greater than state 0, even with a lower diversification rate. 
Frequencies <- c('State 0', 'State 1')
Colors <- c('pink', 'purple')
Data <- matrix(c(0.68, 0.69, 0.57, 0.647, 0.642, 0.43, 0.32, 0.3, 0.43, 0.35, 0.35, 0.568), nrow = 2, ncol = 6, byrow=TRUE)
Data
Difference <- c(0.15, 0.1, 0.05, 0.03, 0.02, 0.01)
Freq1 <- c(0.32, 0.3, 0.43, 0.35, 0.35, 0.568)
Freq0 <- c(0.68, 0.69, 0.57, 0.647, 0.642, 0.43)

pdf('Question1.pdf', height =6, width=6)
barplot(Data, names.arg=Difference,
main = 'Changes in Frequency of States based on Variation in R Values',
xlab = 'Difference in Diversification Rate',
ylab = 'Frequency',
beside=TRUE,
col = c('pink', 'purple')
)
legend('topright', Frequencies, fill = Colors)
dev.off()



# 2. is the frequency of state 1 ever zero when the transition rates are both non-zero?
#I got the transition rates as close to zero as I possibly could while still being able to make a tree. I also found that the way to get state 1 closest to zero was to increase the net diversification rate of state 0, making it a lot larger than the diversification rate of state 1. Even when I ran trees for which one was zero, state 1 was never zero
Frequencies <- c('State 0', 'State 1')
Colors <- c('red', 'blue')
Data <- matrix(c(0.82, 0.8, 0.96, 0.85, 0.63, 0.9, 0.926, 0.923, 0.959, 0.955, 0.945, 0.968, 0.977, 0.963, 0.978, 0.984, 0.973, 0.18, 0.2, 0.04, 0.14, 0.37, 0.088, 0.074, 0.077, 0.041, 0.045, 0.055, 0.032, 0.023, 0.037, 0.022, 0.016, 0.027), nrow = 2, ncol = 17, byrow=TRUE)
Data
Difference <- c(0.05, 0.05, 0, 0, 0, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.45, 0.45, 0.45)
pdf('Question2.pdf', height = 8, width = 8)
barplot(Data, names.arg=Difference,
main='How Close to Zero State 1 Gets When Transition Rate is Nonzero',
xlab='Difference in Diversification Rate',
ylab='Frequencies',
col=c('red', 'blue')
)
legend('topright', Frequencies, fill = Colors)
dev.off()
# 3. how much variation is there in the frequency of state 1 when the parameters are the same?
#I didn't expect to see variation in the frequency of 1 when the parameters were the same but I did. I ran 3 sets of 10 trials, changing the net diversification rate in each set but keeping the parameters the same for state 1 and state 0 to evaluate how state 1 varied across several of the same trials with the same parameters. 
Data <- read.csv('~/Desktop/Evolution/Tasks/Task_10/Question3_Data.csv',
	stringsAsFactors=F)
head(Data)
Freq1_Trial1 <- Data[,2]
Freq1_Trial2 <- Data[,5]
Freq1_Trial3 <- Data[,8]
Variance1 <- var(Freq1_Trial1)
Variance2 <- var(Freq1_Trial2)
Variance3 <- var(Freq1_Trial3)
Variance1
Variance2
Variance3
VarianceMatrix <- c(Variance1, Variance2, Variance3)
VarianceMatrix
Trial <- c(1, 2, 3)
pdf('Question3.pdf', height=8, width=8)
barplot(VarianceMatrix, names.arg=Trial,
	main='Variance of Frequency 1 in Each Trial',
	ylim= c(0, 0.5),
	xlab='Trial Number',
	ylab='Variance in Frequencies',
	col='green')
dev.off()
#This graph shows that there is little to no variance between freqeuncy of 1 when the parameters are the same. 
# 4. what factors other than the rates influence the final freqeuncy of state 1? 
#In a real-world scenario, effects of drift, inbreeding, and selection will change the frequency of state 1. In the case of simulation, the frequency of state 1 is influenced by how the simulation favors each state. The graphs I've already done show varying results of frequencies using the same parameters. Specifically, my graph from question 3 is the best representation of what is going on. 

#### My Pattern ####
# The pattern I decided to look at was how frequencies change if their net diversification rate increases. I expected that more diversification in the values would yield a higher frequency value. 
Data <- read.csv('~/Desktop/Evolution/Tasks/Task_10/My_Own_Trend.csv',
	stringsAsFactors=F)
head(Data)
Freq_0 <- Data[,2]
Freq_0
NDR_0 <- Data[,1]
pdf('OwnTrend1.pdf', height=8, width=8)
plot(NDR_0, Freq_0, xlab='Net Diversification Rate of State 0', ylab='Frequency of State 0', main='How Net Diversification Rate Influences Frequency')
abline(lm(Freq_0~NDR_0), col='blue', lty='dashed')
dev.off()
Freq_1 <- Data[,7]
NDR_1 <- Data[,5]

pdf('OwnTrend2.pdf', height=8, width=8)
plot(NDR_1, Freq_1, xlab='Net Diversification Rate of State 1', ylab='Frequency of State 1', main='How Net Diversification Rate Influences Frequency')
abline(lm(Freq_1~NDR_1), col='blue', lty='dashed')
dev.off()




