setwd('~/Desktop/Evolution/Tasks/Task_06')
#Analyzing allele frequencies from a population of scrub jays. 
#Be careful when examining objects.
#overallFreq() is for examining the frequency of each allele in the population each year.
#alleleFreqs() frequency of each allele in the overall population
#(d_birth) birds born that year
#(d_surv) birds that survived into that year from the previous
#(d_imm) birds that arrived as immigrants
#(n_columns) number of newborns, survivors, and immigrants
#rfreq relative change in the frequency of the locus from the starting frequency
source('http://jonsmitchell.com/code/reformatData07.R')
source('http://jonsmitchell.com/code/simFxn.R')

#make a plot of each allele's frequency over time
plot(1, 1, type='n', xlim=c(1998, 2013), ylim=c(0, 1))
s <- apply(overallFreq, 2, function(x) lines(overallFreq[,1], x, col=rgb(0,0,0,0.01)))

#make a plot of each allele's rescaled frequency (observed - initial) over time. 
rescaleFreq <- apply(overallFreq[,3:ncol(overallFreq)], 2, function(x) x - x[1])
plot(1, 1, type='n', xlim=c(1998, 2013), ylim=c(-0.25, 0.25))
s <- apply(rescaleFreq, 2, function(x) lines(overallFreq[,1], x, col=rgb(0,0,0,0.01)))

# remake the right data
dYear <- c()
dAlleles <- c()

for (i in 3:ncol(overallFreq))	{
	dYear <- c(dYear, overallFreq[,1])
	Vec <- overallFreq[,i]
	Init <- overallFreq[1, i]
	dAlleles <- c(dAlleles, Vec - Init)
}

#instead of plotting individual allele frequencies, now we'll plot the probability of change in freqeuncy (y) by year (x)
smoothScatter(dYear, dAlleles, colramp = Pal, nbin = 100)

#smoothScatter() show a summary of how likely a given change in allele frequency is over a given period of time in this population. Changing the nbin argument can make it more or less pixelated. 
#Using simPop, we can simulate populations and stick them on this graph. 
#addFit() aids us in this task and function runs a specific number of simPop simulations with set parameters. 
#Find the combination of n, h, and s that best matches the empirical data.
pdf('r06-nhs_best_fit.pdf', height = 5, width = 5)
smoothScatter(dYear, dAlleles, colramp = Pal, nbin = 100, xlab = 'year', ylab = 'change in allele freq. since 1998')
addFit(nruns = 100, n = 225, ngens = 18, h =1.25, s = 0.035, startT = 1997, simCol = 'gray40', rescale = TRUE)
dev.off()
plot(alleleFreqs$d_freq, alleleFreqs$d_imm, xlim=c(-0.15, 0.15), xlab='overall freq.change', ylab='freq change in subset')
points(alleleFreqs$d_freq, alleleFreqs$d_birth, col='blue')
points(alleleFreqs$d_freq, alleleFreqs$d_surv, col='red')

#Relative fitness and selection go hand in hand under the relationship: s = 1 - w. Adding fitness to hardy weinberg can be done by multiplying the frequency of each genotype by the fitness of that genotype. Here we are working with allele frequencies. Source ('https://www.radford.edu/~rsheehy/Gen_flash/ABLE_Workshop/Popgen_Equations.pdf') says that we can take hardy weinberg equation and multiply each term by the fitness of that genotype divided by the mean fitness of the three genotypes.


#If a change in allele frequency promotes the ability of an individual to survive into the next generation, then the probability of selection for a trait can be found upon mapping the correlation between survivability and change in allele frequency.
plot(alleleFreqs$d_surv, alleleFreqs$rfreq, xlab ='change in allele freq', ylab = 'selection pressure change')


