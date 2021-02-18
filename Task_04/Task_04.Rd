setwd('~/Desktop/Evolution/Tasks/Task_04')
source('http://jonsmitchell.com/code/fxn05.R')
#simple simulation
Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0)

#plot
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0,1), type = 'l', xlab='generation', ylab='allele freq.', lwd=2 )
lines(1:nrow(Pop1), Pop1[, 2], lwd=2, col='red')
legend('topleft', legend = c('a', 'b'), col = c('black', 'red'), lwd = 2, bty='n')

#Dr. Mitchell set up a plotting wrapper function which works the same as normal simPop except you can set of the number of runs to do multiple simulations at once. 
#It will plot the relative fitnesses of the three genotypes (left), and the frequencies of allele a for each of the nruns populations with a different color for each simulation. 

plotFit( nruns = 10, n=50, ngens=100, init_p=0.5, h=1, s=0)

#This R assignment focuses around the 112 selection toothpick lab but topics that were simplified and are now going to be revisited. 
#A test statistic is a way to quantify how different a set of observations are from your expectation. 
#T test compares differences in the means between two groups.
#Unforunately, random variation samples means that the t test will not be zero even if there is no meaningful difference between groups because the variation will naturally exist. 
#The P value is a measure of the probability that your data differ from your expectation due to sampling errors only, and the problem with this is that it is never true because they are accruate if and only if all of the assumptions of your t-statistic are met. So it's basically a huge contradiction. 
#Standardized Test statistics only make sense in a world where the published tables are your only source to look things up. In this scenario, you wouldn't have an infinite number of published tables and people would only design experiments to fit the tables they had.
#We are going to visualize this concept using R.
#Let's assume that we expect to see four equal categories
Expectation <- c(10, 10, 10, 10)

#Let's pretend what we actually see is four uneven categories
Observed <- c(15, 15, 5, 5)

#We can calculate the Chi-squared statistic using these numbers
Chisq <- sum( ( ( Expectation - Observed)^2/ Expectation))
Chisq

#We can readily visualize this
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ '=' ~.(Chisq)), legend.text=c('expected', 'observed'))

Observed <- c(5, 0, 0, 35)
Observed <- c(2, 3, 10, 30)
Observed <- c(10, 10, 10, 10)
Observed <- c(20, 20, 0, 0)
Observed <- c(40, 0, 0, 0)
#Chi-squared = 10 and this relates to the evenness of the bars because the closer our observed value is to 10 the more even the bars will be presented. 
#We're going to evaluate how well actual freshemn do a physical experiment when compared to hypothetical perfect freshmen perform a simulated experiment. We do this using their calculated chi-square values and comparing them to simulations.
#We take the observed data
#We calculate the test statistics and ask how often we find 'significant' results. 
#We simulate an alternative scenario and compare our observed data to the simulation.
#We use that comparison to ask whether finding significance is acually significant. 

#New R File
setwd('~/Desktop/Evolution/Tasks/Task_04')
source('http://jonsmitchell.com/code/fxn05.R')
results <- read.csv('http://jonsmitchell.com/data/biol112labresults.csv', stringsAsFactors=F)
results
length(results)
nrow(results)
ncol(results)
colnames(results)
head(results)

#Subset the data to have only the right pieces
counts <- results[,c('yellow', 'red', 'green', 'blue', 'black', 'tan')]

#List the background colors (They differ from the toothpicks)
backgrounds <- c('White', 'Red', 'Yellow', 'Green', 'Blue', 'Black')
backgrounds

#We set some (slightly nicer) than default colors and this is optional. 
backgroundCol <- c('white', '#d53e4f', '#fee08b', '#abdda4', '#3288bd', 'black')
backgroundCol
#Now we calculate the Chi-squared statistic for the first row of counts. 
calcChi(counts[1,])
chisq.test(counts[1,])

#Calculate the Chi-squared for ALL of the rows at once
Chisqs <- apply(counts, 1, calcChi)
Chisqs
plotChis(counts)
#When the Chi-squared values are really high, the bars are really uneven and one is way higher than any of the other bars. When Chi-squared is really low (as in almost or zero), the bars are completely even. 
#When your chi-square value is a large difference between what you observe and what you expected then the phenotypes are not evenly distributed but when the Chi-square is low, it means that your expected and observed are the same, so the phenotypes are evenly selected for. However, our chi-square value here is HWE. I have drawn this conclusion because if all of the phenotypes are of equal number in the population, then no phenotype is selected for. This graph is telling us that a low chi-square value means no natural selection.

Avg <- mean(Chisqs)
Avg
#The average Chi squared is 60.99 which is much larger than our critical value obtained from the corn genetics table

#Does the chi-square differ by background?	
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
backgroundAvgs
#The chi-squared does differ by background, because it effects which ones are selected for.

#Our critical value is 11.70 and if our chi squared is more than that then the p value is less than 0.05. This is when we say that the difference between the observation and expected is clear. Now we find out how many show a clear difference. 
propSig <- length(which( Chisqs > 11.70) ) / length(Chisqs)
percSig <- round(100 * propSig)
propSig
percSig
#Given such a large percentage of significance, it is likely that there is another force at work. So we will plot to see. 
par(las=1, mar = c(4,4,1,1), mgp= c(2, 0.5, 0), tck = -0.01, cex.axis = 1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab='', ylab='', type='n', yaxt='n')

#Set y and x axes labels separately
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex =1.75, line=2.5)

#Now for each background, we add a histogram of the data.
counter <- 1
for (i in backgrounds)	{
	Data <- Chisqs[which(results[,3] == i)]
	addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
	counter <- counter + 1
}
#Now we add a line that represents the critical value
abline( v=11.70, lty=2, lwd=2, col='black')
#The more to the right of that line a distribution is, the more often trials on that backrground were significant. The backgrounds where less of the toothpicks could blend in easily had the least distribution to the right. 

#simDraws() simulates the toothpick experiment blindly so that each time 40 of the 60 toothpicks are drawn without regard for color or background
Simulation <- simDraws(10000)

#Now we add our Chi-squareds from the 10,000 simulations to our plot
addHist(Y=7, Dat=Simulation, Color='lightgray')
mtext(side=2, at=7, line=0, 'Simulated')

#Even though we don't know what it will look like when a student follows the directions perfectly, we can simulate it and compare. So we can assign differences in fitness. 
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))

#The above represents no fitness differences, which is a nearly exact match for our 'blind' simulation. Now we will select against one toothpick type. 

Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))
#The distribution moved more to the right of the line. 

#3 toothpick types selected against
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))

#5 selected against
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))

#insane selection 
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1 , 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, 'sel.sem')

#It's clear that none of these match the student generated distributions but they have a combination of both sort of. We can plot what a mixture of our simulations would look like pretty easily. 
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,0,0.25))
#The mixture plot is an exact match to most of the student data, especially yellow. 



#The student-generated data doesn't show strong selection, rather a combination of strong and weak selection. I think this can be attributed to not understanding the directions thoroughly or just not following them. I remember people struggling with this lab when we did it, and I remember my first year being more about wanting to be finished with the lab than actually understanding it. I think this is why you get such a mix of selection strength here. Students could also be biased upon their favorite color or just continously pick from the same position on the paper. However, this could represent how things are done in nature, as a predator might have a favorite food or might only like to hunt in one area. 

#The in class evolutionary processes demonstrated include additive genetic variation from natural selection. As certain colors are selected out, more of the others are left behind. This also plays a role in fitness as selection only sees phenotype and as certain ones are selected, the passing on of genes is random.  So beyond the possibility of error in understanding directions, this is a great example of: Just because phentoype is passed on does not mean that genotype also is. 
#As far as simulation goes, it is strictly selection without variation. 
#In conclusion, the results produced by the students are more accurate in terms of what happens in nature, even if the statistics behind it are too narrow (based on one table from a Corn genetics book), they still hold true in terms of what is actually seen in nature. 
#Therefore, comparing the students numbers to simulated numbers is a lot more intuitive than comparing them to a critical value. The critical value, though, was still helpful in establishing a place on the graph where we could guage how far the distribution moved. Beyond that, it wasn't super useful. 
#I think the X2 values would increase as mutation occurs. 
data(simDraws)


#######EXTRACREDIT
#This is what I did to the simDraws function to make changes. 
simDraws <- function(nruns, ncols=6, nstart=10, nrounds=3, mu=0, twoway=TRUE, w=NULL)	{
	Chiout <- c()
	for (j in 1:nruns)	{
		Start <- rep(1:ncols, nstart)
		Pop <- Start
		for (i in 1:nrounds)	{
			if (is.null(w))	{
				Draws <- sample(Pop, 20, replace = T)
			}
			else if (!is.null(w))	{
				if (length(setdiff(unique(Pop), names(w))) == 0)	{
					Draws <- sample(Pop, 20, replace=T, prob=w[Pop])
				}
				else if (length(setdiff(unique(Pop), names(w))) != 0)	{
					cat("Not enough fitness values! ", setdiff(unique(Pop), names(w)))
				}
			}
			Pop <- sort(c(Draws,Draws,Draws))
		}
		Summary <- c()
		for (k in 1:ncols)	{
			Summary[k] <- length(which(Pop == k))
		}

		Chiout[j] <- sum(((Summary - nstart)^2) / nstart)
	}
	return(Chiout)
	
}

Simulation8 <- simDraws(1e4, mu=2)
addHist(Y=8, Dat=Simulation8, Color=rgb(0,0,0,0.25))
Simulation9 <- simDraws(1e4, mu=300)
addHist(Y=8, Dat=Simulation9, Color=rgb(0,0,0,0.25))
#The distribution gets wider and more to the right when you add more mutations. 

Pop2 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0, mu=0, twoway = TRUE, w = NULL) 
plot(1:nrow(Pop2), Pop2[,1], ylim=c(0,1), type = 'l', xlab='generation', ylab='allele freq.', lwd=2 )
lines(1:nrow(Pop2), Pop2[, 2], lwd=2, col='red')
legend('topleft', legend = c('a', 'b'), col = c('black', 'red'), lwd = 2, bty='n')


Pop3 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0, mu=0.001, twoway = TRUE, w = NULL) 
plot(1:nrow(Pop3), Pop3[,1], ylim=c(0,1), type = 'l', xlab='generation', ylab='allele freq.', lwd=2 )
lines(1:nrow(Pop3), Pop3[, 2], lwd=2, col='red')
legend('topleft', legend = c('a', 'b'), col = c('black', 'red'), lwd = 2, bty='n')

#Mutation makes drift occur faster and it makes Chi-squared values increase. Therefore selection must do the same. Selection speeds up drift and correlates with mutation. This has been demonstrated because selection shifts HWE in the same way that mutation has shown to do so in my graphs. 
